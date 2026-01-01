#!/usr/bin/env python3
"""
Summarize a Samply (Firefox Profiler) profile in a terminal-friendly way.

This lets terminal-only agents *use* the profile data without opening a browser UI.

Example:
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --top 25
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --top 25 \
    --addr2line-binary target/release/pageset_progress
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


def _build_children(prefixes: List[Any], n: int) -> List[List[int]]:
    children: List[List[int]] = [[] for _ in range(n)]
    for i in range(n):
        p = prefixes[i] if i < len(prefixes) else None
        pi = _to_index(p)
        if pi is None:
            continue
        if pi >= n:
            continue
        children[pi].append(i)
    return children


def _postorder_subtree_weights(children: List[List[int]], leaf_weight: List[float]) -> List[float]:
    n = len(children)
    subtree = [0.0] * n
    state = [0] * n  # 0=unvisited, 1=visiting, 2=done
    for root in range(n):
        if state[root] != 0:
            continue
        stack: List[Tuple[int, int]] = [(root, 0)]
        while stack:
            node, idx = stack[-1]
            if state[node] == 0:
                state[node] = 1
            if idx < len(children[node]):
                child = children[node][idx]
                stack[-1] = (node, idx + 1)
                if state[child] == 0:
                    stack.append((child, 0))
                continue
            # finalize
            w = leaf_weight[node]
            for c in children[node]:
                w += subtree[c]
            subtree[node] = w
            state[node] = 2
            stack.pop()
    return subtree


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
        "--thread-index",
        type=int,
        default=None,
        help="Select a specific thread by index instead of auto-picking the heaviest",
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
        help="If set, symbolize `0x...` frames via `llvm-addr2line -e <binary>` (best-effort)",
    )
    args = ap.parse_args()

    profile = _open_profile(args.profile)
    threads = profile.get("threads", []) or []

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

    # leaf weights per stack entry
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

    children = _build_children(prefixes, n_stacks)
    subtree_weight = _postorder_subtree_weights(children, leaf_stack_weight)

    leaf_func = defaultdict(float)
    inclusive_func = defaultdict(float)

    for si in range(n_stacks):
        frame_idx = frames[si] if si < len(frames) else None
        fi = _to_index(frame_idx)
        if fi is None:
            continue
        func_idx = _safe_get(frame_funcs, fi)
        fxi = _to_index(func_idx)
        if fxi is None:
            continue
        lw = leaf_stack_weight[si]
        if lw:
            leaf_func[fxi] += lw
        sw = subtree_weight[si]
        if sw:
            inclusive_func[fxi] += sw

    # Determine which entries we are going to print so we can optionally symbolize addresses in one batch.
    top_leaf_items = sorted(leaf_func.items(), key=lambda kv: kv[1], reverse=True)[: args.top]
    top_inclusive_items = sorted(inclusive_func.items(), key=lambda kv: kv[1], reverse=True)[: args.top]
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
    if args.addr2line_binary:
        # Gather all addresses used by the upcoming output so we can symbolize in a single addr2line call.
        used_funcs: set[int] = set()
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

        symbolizer = _Addr2LineSymbolizer(args.addr2line_binary, repo_root=repo_root)
        symbolizer.preload(addrs)

    def func_label(func_idx: int) -> str:
        raw = _func_label(thread, func_idx)
        if symbolizer is None:
            return _strip_rust_hash(raw)
        # If the profile provided a real symbol name, keep it. Otherwise symbolize `0x...`.
        if _ADDR_RE.match(raw):
            return symbolizer.label_for(raw)
        return _strip_rust_hash(raw)

    print(f"Profile: {args.profile}")
    print(f"Thread:  {_thread_label(thread)}")
    print(f"Samples: {n_samples}  Total weight: {total_weight:.0f}")
    print()

    print("Top functions (SELF / leaf samples):")
    for func_idx, w in top_leaf_items:
        print(f"  {_format_pct(w, total_weight)}  {w:10.0f}  {func_label(func_idx)}")
    print()

    print("Top functions (INCLUSIVE / subtree samples):")
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
