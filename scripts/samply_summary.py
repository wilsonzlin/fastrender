#!/usr/bin/env python3
"""
Summarize a Samply (Firefox Profiler) profile in a terminal-friendly way.

This lets terminal-only agents *use* the profile data without opening a browser UI.

Example:
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --top 25
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --top 25 \
    --addr2line-binary target/release/pageset_progress
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --list-threads
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --thread-index 72 \
    --function-contains fastrender::layout
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --all-threads \
    --max-threads 20 --function-contains fastrender::layout
"""

from __future__ import annotations

import argparse
import gzip
import json
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from typing import Any, Dict, Iterable, List, Optional, Tuple


def _open_profile(path: str) -> Dict[str, Any]:
    if path.endswith(".gz"):
        with gzip.open(path, "rt", encoding="utf-8", errors="replace") as f:
            return json.load(f)
    with open(path, "r", encoding="utf-8", errors="replace") as f:
        return json.load(f)


def _sum_weights(samples: Dict[str, Any]) -> float:
    w = samples.get("weight")
    if isinstance(w, list):
        return float(sum(x for x in w if isinstance(x, (int, float))))
    return float(samples.get("length", 0))


def _thread_label(t: Dict[str, Any]) -> str:
    proc = t.get("processName") or "<?>"
    name = t.get("name") or "<?>"
    pid = t.get("pid")
    tid = t.get("tid")
    return f"{proc}/{name} pid={pid} tid={tid}"


def _repo_root() -> str:
    return os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))


_ADDR_RE = re.compile(r"^0x[0-9a-fA-F]+$")
_RUST_HASH_RE = re.compile(r"::h[0-9a-f]{8,}$")
_BUILD_ID_RE = re.compile(r"Build ID: ([0-9a-fA-F]+)")


def _strip_rust_hash(sym: str) -> str:
    # Rust symbols often end with `::h<hash>`, which makes terminal summaries noisy.
    # Strip it when it looks like a compiler-generated hash suffix.
    if " (" in sym:
        head, tail = sym.split(" (", 1)
        return _RUST_HASH_RE.sub("", head) + " (" + tail
    return _RUST_HASH_RE.sub("", sym)


def _parse_addr2line_loc(loc: str) -> Tuple[str, Optional[str]]:
    # Split `path:line (discriminator N)` into (path, rest).
    base = loc
    suffix = ""
    if " (discriminator" in base:
        base, rest = base.split(" (discriminator", 1)
        suffix = " (discriminator" + rest
    if ":" in base:
        path, line = base.rsplit(":", 1)
        return path, f"{line}{suffix}"
    return base, suffix or None


def _shorten_location(loc: str, repo_root: str) -> str:
    if not loc or loc.startswith("??"):
        return loc or "??:0"

    path, rest = _parse_addr2line_loc(loc)

    if repo_root and os.path.isabs(path) and path.startswith(repo_root + os.sep):
        path = os.path.relpath(path, repo_root)
    elif "/.cargo/registry/src/" in path:
        # /root/.cargo/registry/src/<registry>/<crate>/path -> <crate>/path
        rem = path.split("/.cargo/registry/src/", 1)[1]
        parts = rem.split("/")
        if len(parts) > 1:
            path = "/".join(parts[1:])
    elif "/rust/deps/" in path:
        path = path.split("/rust/deps/", 1)[1]
    elif "/rustc/" in path and "/library/" in path:
        path = path.split("/library/", 1)[1]

    if rest:
        return f"{path}:{rest}"
    return path


def _find_tool(candidates: List[str]) -> Optional[str]:
    for c in candidates:
        tool = shutil.which(c)
        if tool:
            return tool
    return None


def _elf_build_id(path: str) -> Optional[str]:
    if not path or not os.path.exists(path):
        return None
    tool = _find_tool(["readelf", "llvm-readelf"])
    if not tool:
        return None
    try:
        res = subprocess.run(
            [tool, "-n", path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )
    except Exception:
        return None
    for line in (res.stdout or "").splitlines():
        m = _BUILD_ID_RE.search(line)
        if m:
            return m.group(1).lower()
    return None


def _profile_product(profile: Dict[str, Any]) -> Optional[str]:
    meta = profile.get("meta") or {}
    product = meta.get("product")
    return product if isinstance(product, str) and product else None


def _profile_primary_lib(profile: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    product = _profile_product(profile)
    libs = profile.get("libs") or []
    if not product or not isinstance(libs, list):
        return None
    for lib in libs:
        if not isinstance(lib, dict):
            continue
        if lib.get("debugName") == product or lib.get("name") == product:
            return lib
    return None


def _guess_sibling_binary(profile_path: str) -> Optional[str]:
    for suffix in [".profile.json.gz", ".profile.json"]:
        if profile_path.endswith(suffix):
            return profile_path[: -len(suffix)] + ".pageset_progress"
    return None


def _verify_addr2line_binary(
    profile: Dict[str, Any], binary: str
) -> Tuple[Optional[bool], Optional[str], Optional[str]]:
    """
    Returns (matches, build_id, expected_code_id).

    - matches=None means "could not verify" (no build ID tool / not ELF / etc).
    - expected_code_id is taken from the profile's primary lib (meta.product), when available.
    """
    build_id = _elf_build_id(binary)
    if not build_id:
        return None, None, None

    libs = profile.get("libs") or []
    if isinstance(libs, list):
        for lib in libs:
            if not isinstance(lib, dict):
                continue
            code_id = lib.get("codeId")
            if isinstance(code_id, str) and code_id.lower() == build_id:
                return True, build_id, code_id.lower()

    primary = _profile_primary_lib(profile)
    expected = None
    if primary is not None:
        code_id = primary.get("codeId")
        if isinstance(code_id, str) and code_id:
            expected = code_id.lower()
    return False, build_id, expected


def _auto_addr2line_binary(profile_path: str, profile: Dict[str, Any]) -> Optional[str]:
    # 1) Prefer a sibling binary snapshot saved by scripts/profile_samply.sh.
    sibling = _guess_sibling_binary(profile_path)
    if sibling and os.path.exists(sibling):
        return sibling

    # 2) Fall back to the binary path recorded in the profile itself, but only when we can verify
    #    the ELF build ID matches the profile's codeId (so we don't print misleading symbols after
    #    a rebuild).
    primary = _profile_primary_lib(profile)
    if primary is None:
        return None
    cand = primary.get("debugPath") or primary.get("path")
    if not isinstance(cand, str) or not cand or not os.path.exists(cand):
        return None
    code_id = primary.get("codeId")
    build_id = _elf_build_id(cand)
    if (
        isinstance(code_id, str)
        and code_id
        and build_id
        and build_id.lower() == code_id.lower()
    ):
        return cand
    return None


class _Addr2LineSymbolizer:
    def __init__(self, binary: str, repo_root: str) -> None:
        self._binary = binary
        self._repo_root = repo_root
        self._tool = shutil.which("llvm-addr2line")
        self._labels: Dict[str, str] = {}

    def _frame_rank(self, loc: str) -> int:
        if not loc or loc.startswith("??"):
            return 100
        path, _rest = _parse_addr2line_loc(loc)
        if self._repo_root and os.path.isabs(path) and path.startswith(self._repo_root + os.sep):
            # Prefer repo-local code; slightly deprioritize CLIs if there are library frames too.
            if f"{os.sep}src{os.sep}bin{os.sep}" in path:
                return 1
            return 0
        if "/.cargo/registry/src/" in path or "/rust/deps/" in path:
            return 2
        if "/rustc/" in path:
            return 4
        return 3

    def preload(self, addresses: Iterable[str]) -> None:
        if not self._tool:
            return
        if not os.path.exists(self._binary):
            return
        pending = sorted({a for a in addresses if a not in self._labels})
        if not pending:
            return

        # -i is critical: most interesting Rust frames are inlined (especially in release-like builds).
        cmd = [
            self._tool,
            "-f",
            "-C",
            "-i",
            "-a",
            "-e",
            self._binary,
            *pending,
        ]
        try:
            res = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
            )
        except Exception:
            return

        # Parse addr2line output. With `-a`, each address begins with a `0x...` line, followed by
        # pairs of (function, file:line). With `-i`, there may be multiple pairs per address.
        cur_addr: Optional[str] = None
        frames: List[Tuple[str, str]] = []

        def flush() -> None:
            nonlocal cur_addr, frames
            if cur_addr is None:
                return
            label = cur_addr
            if frames:
                best_i = 0
                best_rank = 10**9
                for i, (_fn, loc) in enumerate(frames):
                    r = self._frame_rank(loc)
                    # Prefer outer frames (higher i) when rank ties: it tends to show a more actionable
                    # algorithm-level function instead of a tiny inlined helper.
                    if r < best_rank or (r == best_rank and i > best_i):
                        best_rank = r
                        best_i = i
                fn, loc = frames[best_i]
                fn = _strip_rust_hash(fn)
                loc = _shorten_location(loc, self._repo_root)
                if fn != "??" or not loc.startswith("??"):
                    if loc and not loc.startswith("??"):
                        label = f"{fn} ({loc})"
                    else:
                        label = fn
            self._labels[cur_addr] = label
            cur_addr = None
            frames = []

        lines = (res.stdout or "").splitlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            if _ADDR_RE.match(line):
                flush()
                cur_addr = line
                frames = []
                i += 1
                continue

            # (function, location) pair
            fn = line
            loc = lines[i + 1].strip() if i + 1 < len(lines) else "??:0"
            frames.append((fn, loc))
            i += 2
        flush()

        # Ensure we have *something* for every pending address.
        for a in pending:
            self._labels.setdefault(a, a)

    def label_for(self, addr: str) -> str:
        return self._labels.get(addr, addr)


def _pick_thread(profile: Dict[str, Any], contains: Optional[str]) -> Optional[Dict[str, Any]]:
    best: Optional[Dict[str, Any]] = None
    best_weight = -1.0
    for t in profile.get("threads", []) or []:
        samples = t.get("samples") or {}
        weight = _sum_weights(samples)
        if weight <= 0:
            continue
        if contains:
            hay = f"{t.get('processName','')} {t.get('name','')}"
            if contains not in hay:
                continue
        if weight > best_weight:
            best = t
            best_weight = weight
    return best


def _thread_matches(t: Dict[str, Any], contains: Optional[str]) -> bool:
    if not contains:
        return True
    hay = f"{t.get('processName','')} {t.get('name','')}"
    return contains in hay


def _to_index(value: Any) -> Optional[int]:
    if value is None:
        return None
    if isinstance(value, bool):
        return None
    if isinstance(value, int):
        return value if value >= 0 else None
    if isinstance(value, float):
        # Some profiles encode IDs as floats (e.g. 12345.1). Treat them as non-indices.
        return None
    return None


def _safe_get(arr: List[Any], idx: Any) -> Any:
    i = _to_index(idx)
    if i is None or i >= len(arr):
        return None
    return arr[i]


def _decode_string(strings: List[str], idx: Any) -> str:
    i = _to_index(idx)
    if i is None or i >= len(strings):
        return "<?>"
    s = strings[i]
    if not isinstance(s, str) or not s:
        return "<?>"
    return s


def _func_label(thread: Dict[str, Any], func_idx: int) -> str:
    strings = thread.get("stringArray") or []
    func_table = thread.get("funcTable") or {}
    name_idx = _safe_get(func_table.get("name") or [], func_idx)
    file_idx = _safe_get(func_table.get("fileName") or [], func_idx)
    line = _safe_get(func_table.get("lineNumber") or [], func_idx)
    name = _decode_string(strings, name_idx)
    file_name = _decode_string(strings, file_idx) if file_idx is not None else ""
    if isinstance(line, (int, float)) and file_name:
        return f"{name} ({file_name}:{int(line)})"
    if file_name:
        return f"{name} ({file_name})"
    return name


def _func_table_len(thread: Dict[str, Any]) -> int:
    func_table = thread.get("funcTable") or {}
    n = func_table.get("length")
    if isinstance(n, int) and n > 0:
        return n
    # Fall back to the name column length, which should match func count in all supported profiles.
    names = func_table.get("name") or []
    return len(names) if isinstance(names, list) else 0


def _thread_function_weights(
    thread: Dict[str, Any],
) -> Tuple[
    Dict[int, float],
    List[float],
    List[float],
    float,
    Dict[str, Any],
    List[Any],
    List[Any],
    List[Any],
]:
    """
    Compute (
        leaf_by_func_idx,
        inclusive_unique_by_func_idx,
        leaf_stack_weight,
        total_weight,
        stack_table,
        prefixes,
        frames,
        frame_funcs,
    ).

    `inclusive_unique_by_func_idx` is inclusive weight where each function is counted at most once per unique stack.
    """
    samples = thread.get("samples") or {}
    n_samples = int(samples.get("length", 0) or 0)
    sample_stacks = samples.get("stack") or []
    sample_weights = samples.get("weight") or []
    total_weight = _sum_weights(samples)

    stack_table = thread.get("stackTable") or {}
    n_stacks = int(stack_table.get("length", 0) or 0)
    prefixes = stack_table.get("prefix") or []
    frames = stack_table.get("frame") or []

    frame_table = thread.get("frameTable") or {}
    frame_funcs = frame_table.get("func") or []

    n_funcs = _func_table_len(thread)

    leaf_stack_weight = [0.0] * n_stacks
    for i in range(n_samples):
        s = sample_stacks[i] if i < len(sample_stacks) else None
        si = _to_index(s)
        if si is None or si >= n_stacks:
            continue
        w = sample_weights[i] if i < len(sample_weights) else 1
        if not isinstance(w, (int, float)):
            w = 1
        leaf_stack_weight[si] += float(w)

    leaf_func: Dict[int, float] = defaultdict(float)
    for si in range(n_stacks):
        lw = leaf_stack_weight[si]
        if not lw:
            continue
        frame_idx = frames[si] if si < len(frames) else None
        fi = _to_index(frame_idx)
        if fi is None:
            continue
        func_idx = _safe_get(frame_funcs, fi)
        fxi = _to_index(func_idx)
        if fxi is None:
            continue
        leaf_func[fxi] += lw

    inclusive_unique = [0.0] * n_funcs
    if n_funcs:
        seen_marker: List[int] = [0] * n_funcs
        for stack_i, weight in enumerate(leaf_stack_weight):
            if weight <= 0:
                continue
            mark = stack_i + 1
            cur = stack_i
            while True:
                frame_idx = frames[cur] if cur < len(frames) else None
                fi = _to_index(frame_idx)
                func_idx = _safe_get(frame_funcs, fi) if fi is not None else None
                fxi = _to_index(func_idx)
                if fxi is not None and fxi < n_funcs:
                    if seen_marker[fxi] != mark:
                        seen_marker[fxi] = mark
                        inclusive_unique[fxi] += weight

                p = prefixes[cur] if cur < len(prefixes) else None
                cur = _to_index(p)
                if cur is None or cur >= n_stacks:
                    break

    return (
        leaf_func,
        inclusive_unique,
        leaf_stack_weight,
        total_weight,
        stack_table,
        prefixes,
        frames,
        frame_funcs,
    )


def _format_pct(x: float, total: float) -> str:
    if total <= 0:
        return "0.00%"
    return f"{(x * 100.0 / total):6.2f}%"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("profile", help="Path to samply profile.json(.gz)")
    ap.add_argument("--top", type=int, default=25, help="Top N entries to show")
    ap.add_argument(
        "--thread-contains",
        default=None,
        help="Only consider threads whose process/name contains this substring",
    )
    ap.add_argument(
        "--max-stack-frames",
        type=int,
        default=35,
        help="Max frames to print for top stacks",
    )
    ap.add_argument(
        "--function-contains",
        default=None,
        help="Only include functions whose label contains this substring (applies to printed tables)",
    )
    ap.add_argument(
        "--thread-index",
        type=int,
        default=None,
        help="Select a specific thread by index instead of auto-picking the heaviest",
    )
    ap.add_argument(
        "--all-threads",
        action="store_true",
        help="Aggregate top functions across all matching threads (useful when CPU is spread across worker threads)",
    )
    ap.add_argument(
        "--max-threads",
        type=int,
        default=None,
        help="When using --all-threads, consider at most this many heaviest threads (default: all)",
    )
    ap.add_argument(
        "--list-threads",
        action="store_true",
        help="List threads by sample weight and exit (use with --thread-index)",
    )
    ap.add_argument(
        "--threads-top",
        type=int,
        default=20,
        help="When listing threads, show at most this many (default: 20)",
    )
    ap.add_argument(
        "--addr2line-binary",
        default=None,
        help=(
            "Symbolize `0x...` frames via `llvm-addr2line -e <binary>` (best-effort). "
            "If omitted, tries to auto-detect a sibling `<profile>.pageset_progress` snapshot "
            "(saved by scripts/profile_samply.sh) or the binary path recorded in the profile "
            "(only when ELF build IDs match)."
        ),
    )
    args = ap.parse_args()

    profile = _open_profile(args.profile)
    threads = profile.get("threads", []) or []

    addr2line_binary = args.addr2line_binary
    if addr2line_binary is None:
        addr2line_binary = _auto_addr2line_binary(args.profile, profile)
    if addr2line_binary:
        matches, build_id, expected = _verify_addr2line_binary(profile, addr2line_binary)
        if matches is False:
            exp = expected or "<unknown>"
            msg = (
                f"WARNING: --addr2line-binary build-id {build_id} does not match profile codeId {exp}; "
                "symbols may be incorrect."
            )
            hint = _guess_sibling_binary(args.profile)
            if hint:
                msg += f" (hint: try {hint})"
            if args.addr2line_binary is None:
                # Auto-detected, but verification failed: don't print misleading symbols.
                print(msg + " (auto-detected binary rejected)", file=sys.stderr)
                addr2line_binary = None
            else:
                print(msg, file=sys.stderr)

    if args.list_threads:
        weighted = []
        for i, t in enumerate(threads):
            w = _sum_weights(t.get("samples") or {})
            if w > 0:
                weighted.append((w, i, t))
        weighted.sort(reverse=True)
        print(f"Profile: {args.profile}")
        print(f"Threads with samples: {len(weighted)} / {len(threads)} total")
        print()
        print(f"Top threads (by sample weight, top {min(args.threads_top, len(weighted))}):")
        for w, i, t in weighted[: args.threads_top]:
            print(f"  [{i:3d}] {w:10.0f}  {_thread_label(t)}")
        print()
        return 0

    if args.all_threads:
        weighted = []
        for i, t in enumerate(threads):
            if not _thread_matches(t, args.thread_contains):
                continue
            w = _sum_weights(t.get("samples") or {})
            if w > 0:
                weighted.append((w, i, t))
        weighted.sort(reverse=True)
        if args.max_threads is not None:
            weighted = weighted[: max(0, args.max_threads)]

        if not weighted:
            print("No matching threads with samples found.")
            return 1

        total_weight_all = float(sum(w for w, _i, _t in weighted))

        leaf_all = defaultdict(float)
        inclusive_all = defaultdict(float)
        for _w, _i, t in weighted:
            leaf_func, inclusive_unique, _leaf_stack_weight, _tw, _stack_table, _prefixes, _frames, _frame_funcs = (
                _thread_function_weights(t)
            )
            for func_idx, w in leaf_func.items():
                label = _strip_rust_hash(_func_label(t, func_idx))
                if label != "<?>":
                    leaf_all[label] += w
            for func_idx, w in enumerate(inclusive_unique):
                if w <= 0:
                    continue
                label = _strip_rust_hash(_func_label(t, func_idx))
                if label != "<?>":
                    inclusive_all[label] += w

        repo_root = _repo_root()
        symbolizer: Optional[_Addr2LineSymbolizer] = None
        if addr2line_binary:
            symbolizer = _Addr2LineSymbolizer(addr2line_binary, repo_root=repo_root)
            if args.function_contains:
                # When filtering on a substring we need symbolized labels to match `fastrender::...`
                # even when the profile only recorded raw addresses for some frames.
                addrs = {
                    label
                    for label in set(leaf_all.keys()) | set(inclusive_all.keys())
                    if _ADDR_RE.match(label)
                }
                symbolizer.preload(addrs)

        def pretty(label: str) -> str:
            if symbolizer is None:
                return label
            if _ADDR_RE.match(label):
                return symbolizer.label_for(label)
            return label

        if args.function_contains:
            needle = args.function_contains

            leaf_pretty = defaultdict(float)
            for label, w in leaf_all.items():
                leaf_pretty[pretty(label)] += w
            inclusive_pretty = defaultdict(float)
            for label, w in inclusive_all.items():
                inclusive_pretty[pretty(label)] += w

            top_leaf = sorted(
                [(label, w) for label, w in leaf_pretty.items() if needle in label],
                key=lambda kv: kv[1],
                reverse=True,
            )[: args.top]
            top_inclusive = sorted(
                [(label, w) for label, w in inclusive_pretty.items() if needle in label],
                key=lambda kv: kv[1],
                reverse=True,
            )[: args.top]
        else:
            top_leaf = sorted(leaf_all.items(), key=lambda kv: kv[1], reverse=True)[: args.top]
            top_inclusive = sorted(inclusive_all.items(), key=lambda kv: kv[1], reverse=True)[: args.top]
            if symbolizer is not None:
                # Symbolize just what we're going to print.
                addrs = {
                    label
                    for label, _w in top_leaf + top_inclusive
                    if _ADDR_RE.match(label)
                }
                symbolizer.preload(addrs)

        print(f"Profile: {args.profile}")
        print(f"Threads: {len(weighted)} (filtered) / {len(threads)} total")
        print(f"Total weight: {total_weight_all:.0f}")
        print()

        print("Top functions (SELF / leaf samples, all threads):")
        for label, w in top_leaf:
            shown = label if args.function_contains else pretty(label)
            print(f"  {_format_pct(w, total_weight_all)}  {w:10.0f}  {shown}")
        print()

        print("Top functions (INCLUSIVE / de-duped stacks, all threads):")
        for label, w in top_inclusive:
            shown = label if args.function_contains else pretty(label)
            print(f"  {_format_pct(w, total_weight_all)}  {w:10.0f}  {shown}")
        print()
        return 0

    thread: Optional[Dict[str, Any]] = None
    if args.thread_index is not None:
        if args.thread_index < 0 or args.thread_index >= len(threads):
            print(f"--thread-index {args.thread_index} out of range (threads={len(threads)})")
            return 2
        thread = threads[args.thread_index]
    else:
        thread = _pick_thread(profile, args.thread_contains)

    if thread is None:
        print("No threads with samples found.")
        return 1

    leaf_func, inclusive_unique, leaf_stack_weight, total_weight, stack_table, prefixes, frames, frame_funcs = (
        _thread_function_weights(thread)
    )
    n_samples = int((thread.get("samples") or {}).get("length", 0) or 0)

    # Determine which entries we are going to print so we can optionally symbolize addresses in one batch.
    leaf_items_all = sorted(leaf_func.items(), key=lambda kv: kv[1], reverse=True)
    inclusive_items_all = sorted(
        [(i, w) for i, w in enumerate(inclusive_unique) if w > 0],
        key=lambda kv: kv[1],
        reverse=True,
    )
    top_leaf_items = leaf_items_all[: args.top]
    top_inclusive_items = inclusive_items_all[: args.top]
    top_stack_entries = sorted(
        [(w, si) for si, w in enumerate(leaf_stack_weight) if w > 0],
        key=lambda x: x[0],
        reverse=True,
    )[: min(5, args.top)]

    # Precompute the stack chains so we can both symbolize and later print without re-walking prefixes.
    stack_chains: List[Tuple[float, int, List[int], bool]] = []
    for w, si in top_stack_entries:
        chain: List[int] = []
        cur = si
        while True:
            chain.append(cur)
            p = prefixes[cur] if cur < len(prefixes) else None
            pi = _to_index(p)
            if pi is None:
                break
            cur = pi
            if len(chain) > 4096:
                break
        chain.reverse()
        truncated = False
        if len(chain) > args.max_stack_frames:
            chain = chain[-args.max_stack_frames :]
            truncated = True
        stack_chains.append((w, si, chain, truncated))

    repo_root = _repo_root()
    symbolizer: Optional[_Addr2LineSymbolizer] = None
    if addr2line_binary:
        # Gather all addresses used by the upcoming output so we can symbolize in a single addr2line call.
        used_funcs: set[int] = set()
        if args.function_contains:
            used_funcs.update(leaf_func.keys())
            used_funcs.update(i for i, w in enumerate(inclusive_unique) if w > 0)
        else:
            for func_idx, _w in top_leaf_items:
                used_funcs.add(func_idx)
            for func_idx, _w in top_inclusive_items:
                used_funcs.add(func_idx)
        for _w, _si, chain, _trunc in stack_chains:
            for sidx in chain:
                frame_idx = frames[sidx] if sidx < len(frames) else None
                fi = _to_index(frame_idx)
                func_idx = _safe_get(frame_funcs, fi) if fi is not None else None
                fxi = _to_index(func_idx)
                if fxi is not None:
                    used_funcs.add(fxi)

        strings = thread.get("stringArray") or []
        func_table = thread.get("funcTable") or {}
        names = func_table.get("name") or []
        addrs: set[str] = set()
        for func_idx in used_funcs:
            name_idx = _safe_get(names, func_idx)
            name = _decode_string(strings, name_idx)
            if _ADDR_RE.match(name):
                addrs.add(name)

        symbolizer = _Addr2LineSymbolizer(addr2line_binary, repo_root=repo_root)
        symbolizer.preload(addrs)

    def func_label(func_idx: int) -> str:
        raw = _func_label(thread, func_idx)
        if symbolizer is None:
            return _strip_rust_hash(raw)
        # If the profile provided a real symbol name, keep it. Otherwise symbolize `0x...`.
        if _ADDR_RE.match(raw):
            return symbolizer.label_for(raw)
        return _strip_rust_hash(raw)

    if args.function_contains:
        needle = args.function_contains
        top_leaf_items = [
            (func_idx, w)
            for func_idx, w in leaf_items_all
            if needle in func_label(func_idx)
        ][: args.top]
        top_inclusive_items = [
            (func_idx, w)
            for func_idx, w in inclusive_items_all
            if needle in func_label(func_idx)
        ][: args.top]

    print(f"Profile: {args.profile}")
    print(f"Thread:  {_thread_label(thread)}")
    print(f"Samples: {n_samples}  Total weight: {total_weight:.0f}")
    print()

    print("Top functions (SELF / leaf samples):")
    for func_idx, w in top_leaf_items:
        print(f"  {_format_pct(w, total_weight)}  {w:10.0f}  {func_label(func_idx)}")
    print()

    print("Top functions (INCLUSIVE / per-sample stacks, de-duped):")
    for func_idx, w in top_inclusive_items:
        print(f"  {_format_pct(w, total_weight)}  {w:10.0f}  {func_label(func_idx)}")
    print()

    if stack_chains:
        print("Top call stacks (by leaf samples):")
        for w, si, chain, truncated in stack_chains:
            print(f"- {_format_pct(w, total_weight)} {w:.0f} samples  stack#{si}")
            if truncated:
                print("    ...")
            for sidx in chain:
                frame_idx = frames[sidx] if sidx < len(frames) else None
                fi = _to_index(frame_idx)
                func_idx = _safe_get(frame_funcs, fi) if fi is not None else None
                fxi = _to_index(func_idx)
                label = func_label(fxi) if fxi is not None else "<?>"
                print(f"    {label}")
        print()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
