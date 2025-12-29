#!/usr/bin/env python3
"""
Summarize a Samply (Firefox Profiler) profile in a terminal-friendly way.

This lets terminal-only agents *use* the profile data without opening a browser UI.

Example:
  python3 scripts/samply_summary.py target/pageset/profiles/example.profile.json.gz --top 25
"""

from __future__ import annotations

import argparse
import gzip
import json
import os
import sys
from collections import defaultdict
from typing import Any, Dict, List, Optional, Tuple


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
    args = ap.parse_args()

    profile = _open_profile(args.profile)
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

    print(f"Profile: {args.profile}")
    print(f"Thread:  {_thread_label(thread)}")
    print(f"Samples: {n_samples}  Total weight: {total_weight:.0f}")
    print()

    def print_table(title: str, counts: Dict[int, float]) -> None:
        items = sorted(counts.items(), key=lambda kv: kv[1], reverse=True)[: args.top]
        print(title)
        for func_idx, w in items:
            print(f"  {_format_pct(w, total_weight)}  {w:10.0f}  {_func_label(thread, func_idx)}")
        print()

    print_table("Top functions (SELF / leaf samples):", leaf_func)
    print_table("Top functions (INCLUSIVE / subtree samples):", inclusive_func)

    # Show a few hottest call stacks by leaf stack weight.
    top_stack_entries = sorted(
        [(w, si) for si, w in enumerate(leaf_stack_weight) if w > 0],
        key=lambda x: x[0],
        reverse=True,
    )[: min(5, args.top)]

    if top_stack_entries:
        print("Top call stacks (by leaf samples):")
        for w, si in top_stack_entries:
            print(f"- {_format_pct(w, total_weight)} {w:.0f} samples  stack#{si}")
            # unwind
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

            # truncate
            if len(chain) > args.max_stack_frames:
                chain = chain[-args.max_stack_frames :]
                print("    ...")

            for sidx in chain:
                frame_idx = frames[sidx] if sidx < len(frames) else None
                fi = _to_index(frame_idx)
                func_idx = _safe_get(frame_funcs, fi) if fi is not None else None
                fxi = _to_index(func_idx)
                label = _func_label(thread, fxi) if fxi is not None else "<?>"
                print(f"    {label}")
        print()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

