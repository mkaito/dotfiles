#!/usr/bin/env python3
"""
Dump KSP engine data (post-ModuleManager) to CSV.

Usage:
  python dump_ksp_engines.py /path/to/KSP/GameData [output.csv]

What it does:
  - Reads GameData/ModuleManager.ConfigCache (final patched configs).
  - Extracts PARTs that contain ModuleEngines/ModuleEnginesFX modules.
  - Emits one CSV row per engine mode (handles MultiModeEngine).
  - Columns: part_name, title, manufacturer, category, tech_required, entry_cost,
             cost, mass_t, engine_module, engine_id, propellants, engine_type,
             max_thrust_kN, isp_vac_s, isp_sl_s, thrust_vac_kN, thrust_sl_kN

Notes:
  - In KSP, 'maxThrust' is effectively the vacuum thrust for ModuleEngines*.
    Sea-level thrust is: thrust_SL = maxThrust * (Isp_SL / Isp_VAC).
  - This script computes thrust_vac_kN = maxThrust and thrust_sl_kN using the formula above
    when both Isp points are available.
  - If ModuleManager.ConfigCache is missing, run KSP once (with ModuleManager installed)
    so it can generate the cache.
"""

import sys
import os
import re
import csv
import argparse
from typing import List, Dict, Optional, Tuple

def find_config_cache(path: str) -> str:
    if os.path.isdir(path):
        candidate = os.path.join(path, "ModuleManager.ConfigCache")
        if os.path.isfile(candidate):
            return candidate
        # Also allow passing KSP root; then append GameData/
        gd = os.path.join(path, "GameData")
        candidate = os.path.join(gd, "ModuleManager.ConfigCache")
        if os.path.isfile(candidate):
            return candidate
        raise FileNotFoundError("Could not find ModuleManager.ConfigCache under the given directory.")
    else:
        # Direct file path
        if os.path.basename(path).startswith("ModuleManager.") and os.path.isfile(path):
            return path
        raise FileNotFoundError("Path is not a directory or a ModuleManager.ConfigCache file.")

# --- Simple balanced-brace block extraction helpers ---

def strip_line_comments(s: str) -> str:
    """Remove // comments (not inside quotes)."""
    out = []
    i = 0
    in_quote = False
    while i < len(s):
        ch = s[i]
        if ch == '"':
            in_quote = not in_quote
            out.append(ch)
            i += 1
            continue
        if not in_quote and ch == '/' and i + 1 < len(s) and s[i+1] == '/':
            break  # rest of the line is comment
        out.append(ch)
        i += 1
    return ''.join(out)

def find_blocks(text: str, node_name: str) -> List[Tuple[int,int]]:
    """Return [(start_index, end_index)] for each top-level 'node_name { ... }' block."""
    pattern = re.compile(rf'^\s*{re.escape(node_name)}\s*\{{', re.MULTILINE)
    blocks = []
    for m in pattern.finditer(text):
        start = m.start()
        # Find matching closing brace with a stack
        i = m.end() - 1  # at '{'
        depth = 0
        while i < len(text):
            if text[i] == '{':
                depth += 1
            elif text[i] == '}':
                depth -= 1
                if depth == 0:
                    blocks.append((start, i+1))
                    break
            i += 1
    return blocks

def get_field(block: str, key: str) -> Optional[str]:
    m = re.search(rf'^\s*{re.escape(key)}\s*=\s*(.+)$', block, re.MULTILINE)
    if m:
        return m.group(1).strip()
    return None

def find_child_blocks(block: str, child_name: str) -> List[str]:
    """Return list of raw strings for 'child_name { ... }' blocks inside a parent block."""
    results = []
    pattern = re.compile(rf'^\s*{re.escape(child_name)}\s*\{{', re.MULTILINE)
    for m in pattern.finditer(block):
        start = m.start()
        i = m.end() - 1
        depth = 0
        while i < len(block):
            if block[i] == '{':
                depth += 1
            elif block[i] == '}':
                depth -= 1
                if depth == 0:
                    results.append(block[start:i+1])
                    break
            i += 1
    return results

def parse_atmo_curve(module_block: str) -> Tuple[Optional[float], Optional[float]]:
    """
    Return (isp_vac, isp_sl) from atmosphereCurve { key = <p> <isp> ... }
    where p=0 is vacuum, p=1 is sea level. Some parts may omit one key.
    """
    vac = None
    sl = None
    curves = find_child_blocks(module_block, "atmosphereCurve")
    if not curves:
        return vac, sl
    curve = curves[0]
    # lines like: key = 0 345    or  key = 1 85  or key = 0 300 0 0
    for m in re.finditer(r'^\s*key\s*=\s*([0-9.\-eE]+)\s+([0-9.\-eE]+)', curve, re.MULTILINE):
        p = float(m.group(1))
        isp = float(m.group(2))
        if abs(p) < 1e-6:
            vac = isp
        elif abs(p - 1.0) < 1e-6:
            sl = isp
    return vac, sl

def parse_propellants(module_block: str) -> List[str]:
    props = []
    for pblock in find_child_blocks(module_block, "PROPELLANT"):
        # Try resourceName first (some mods use it), else 'name'
        nm = get_field(pblock, "resourceName") or get_field(pblock, "name")
        if nm:
            props.append(nm.strip())
    return props

def is_engine_module(module_block: str) -> bool:
    name = get_field(module_block, "name")
    return name in ("ModuleEngines", "ModuleEnginesFX")

def parse_engines_from_part(part_block: str) -> List[Dict[str, str]]:
    """Extract one dict per engine (per mode)."""
    engines = []
    # Detect MultiModeEngine for labelling
    mm_blocks = find_child_blocks(part_block, "MODULE")
    primary_id = None
    secondary_id = None
    for m in mm_blocks:
        if get_field(m, "name") == "MultiModeEngine":
            primary_id = get_field(m, "primaryEngineID")
            secondary_id = get_field(m, "secondaryEngineID")
            break

    for m in find_child_blocks(part_block, "MODULE"):
        if not is_engine_module(m):
            continue
        engine_id = get_field(m, "engineID") or ""
        eng_type = get_field(m, "EngineType") or ""
        max_thrust = get_field(m, "maxThrust")
        try:
            max_thrust_val = float(max_thrust) if max_thrust is not None else None
        except ValueError:
            max_thrust_val = None
        isp_vac, isp_sl = parse_atmo_curve(m)
        props = parse_propellants(m)

        # Label mode if MultiMode is present
        mode = ""
        if primary_id or secondary_id:
            if engine_id == primary_id:
                mode = "primary"
            elif engine_id == secondary_id:
                mode = "secondary"
            elif engine_id:
                mode = engine_id  # other named ID
            else:
                mode = "multimode-unnamed"
        else:
            mode = engine_id or ""

        # Compute thrusts
        thrust_vac = max_thrust_val
        thrust_sl = None
        if max_thrust_val is not None and isp_vac and isp_sl:
            thrust_sl = max_thrust_val * (isp_sl / isp_vac)

        engines.append({
            "engine_module": get_field(m, "name") or "",
            "engine_id": engine_id,
            "engine_type": eng_type,
            "propellants": "+".join(props),
            "max_thrust_kN": f"{max_thrust_val:.3f}" if max_thrust_val is not None else "",
            "isp_vac_s": f"{isp_vac:.3f}" if isp_vac is not None else "",
            "isp_sl_s": f"{isp_sl:.3f}" if isp_sl is not None else "",
            "thrust_vac_kN": f"{thrust_vac:.3f}" if thrust_vac is not None else "",
            "thrust_sl_kN": f"{thrust_sl:.3f}" if thrust_sl is not None else "",
            "mode_label": mode,
        })
    return engines

def parse_unlocked_techs(save_path: str) -> set:
    """
    Parse a KSP .sfs save file to find unlocked tech node IDs.
    Looks under SCENARIO{name=ResearchAndDevelopment} -> Tech { id=..., state=... }.
    Any Tech with state not explicitly "Unavailable" is treated as unlocked.
    """
    with open(save_path, "r", encoding="utf-8", errors="ignore") as f:
        txt = f.read()
    # Strip // comments for safety
    txt = "\n".join(strip_line_comments(line) for line in txt.splitlines())
    unlocked = set()
    # Find R&D SCENARIO blocks
    for s_start, s_end in find_blocks(txt, "SCENARIO"):
        sblock = txt[s_start:s_end]
        name = get_field(sblock, "name")
        if (name or "").strip() != "ResearchAndDevelopment":
            continue
        # Inside, Tech { id = ..., state = ... }
        for tblock in find_child_blocks(sblock, "Tech"):
            tid = get_field(tblock, "id")
            state = (get_field(tblock, "state") or "").strip()
            if not tid:
                continue
            # Consider unlocked if state is common unlocked labels, or not "Unavailable"
            if state.lower() in {"available", "purchased", "complete", "unlocked", "research", "researched"} or state.lower() != "unavailable":
                unlocked.add(tid.strip())
    # 'start' is always available in Career/Science
    unlocked.add("start")
    return unlocked

def main():
    ap = argparse.ArgumentParser(description="Dump KSP engines (post-ModuleManager) to CSV, optionally filtering by unlocked techs from a save file.")
    ap.add_argument("gamedata_or_cache", help="Path to GameData/ or to ModuleManager.ConfigCache")
    ap.add_argument("-o", "--output", default="engines.csv", help="Output CSV path (default: engines.csv)")
    ap.add_argument("--save", help="Path to a KSP .sfs save (e.g., saves/<game>/persistent.sfs) to filter by unlocked techs")
    ap.add_argument("--include-locked", action="store_true", help="If provided with --save, include locked tech parts as well (adds a column)")
    ap.add_argument("--include-nonengine", action="store_true", help="Also include parts outside Engine/Propulsion categories")
    args = ap.parse_args()

    in_path = args.gamedata_or_cache
    out_csv = args.output
    save_path = args.save

    cache_path = find_config_cache(in_path)
    unlocked_techs = set()
    if save_path and os.path.isfile(save_path):
        try:
            unlocked_techs = parse_unlocked_techs(save_path)
        except Exception as e:
            print(f"Warning: failed to parse save file: {e}")
    with open(cache_path, "r", encoding="utf-8", errors="ignore") as f:
        raw = f.read()

    # Remove line comments to avoid confusing brace matching
    raw_no_comments = "\n".join(strip_line_comments(line) for line in raw.splitlines())

    # Extract PART blocks
    parts = [raw_no_comments[s:e] for (s, e) in find_blocks(raw_no_comments, "PART")]
    rows = []

    for part in parts:
        # Only consider parts that actually have an engine module
        if not any(is_engine_module(mb) for mb in find_child_blocks(part, "MODULE")):
            continue

        part_name = get_field(part, "name") or ""
        title = get_field(part, "title") or ""
        manufacturer = get_field(part, "manufacturer") or ""
        category = get_field(part, "category") or ""
        tech_required = get_field(part, "TechRequired") or ""
        entry_cost = get_field(part, "entryCost") or ""
        cost = get_field(part, "cost") or ""
        mass_str = get_field(part, "mass") or ""
        # Default: only include categories Engine/Propulsion, unless overridden
        if not args.include_nonengine:
            if (category or "").strip().lower() not in {"engine", "propulsion"}:
                continue
        try:
            mass_t = float(mass_str) if mass_str else None
        except ValueError:
            mass_t = None

        engines = parse_engines_from_part(part)
        for e in engines:
            tech_required = tech_required
            is_unlocked = (not unlocked_techs) or (tech_required == "" or tech_required in unlocked_techs)
            if unlocked_techs and not (args.include_locked or is_unlocked):
                continue
            rows.append({
                "title": title,
                "mode": e["mode_label"],
                "propellants": e["propellants"],
                "isp_vac_s": e["isp_vac_s"],
                "isp_sl_s": e["isp_sl_s"],
                "max_thrust_kN": e["max_thrust_kN"],
                "thrust_sl_kN": e["thrust_sl_kN"],
                "mass_t": f"{mass_t:.3f}" if mass_t is not None else "",
                "tech_required": tech_required,
                "unlocked": ("yes" if ((not unlocked_techs) or (tech_required == "" or tech_required in unlocked_techs)) else "no"),
            })

    # Write CSV
    fieldnames = [
        "title","mode","propellants","isp_vac_s","isp_sl_s","max_thrust_kN","thrust_sl_kN","mass_t","tech_required","unlocked"
    ]
    with open(out_csv, "w", newline="", encoding="utf-8") as cf:
        writer = csv.DictWriter(cf, fieldnames=fieldnames)
        writer.writeheader()
        for r in rows:
            writer.writerow(r)

    print(f"Wrote {len(rows)} rows to {out_csv}")
    print("Tip: thrust_SL = max_thrust * (Isp_SL / Isp_VAC). Use 'tech_required' to filter by what you've unlocked.")

if __name__ == "__main__":
    main()
