#!/usr/bin/env python3
from __future__ import annotations

import argparse
import gzip
import io
import json
import re
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple, TypedDict
import xml.etree.ElementTree as ET

# Defaults
DEFAULT_WORKSHOP_CONTENT = Path(
    "/mnt/steamone/SteamLibrary/steamapps/workshop/content/244850"
)
DEFAULT_BASE_DATA = Path(
    "/mnt/steamone/SteamLibrary/steamapps/common/SpaceEngineers/Content/Data"
)
DEFAULT_INDEX_PATH = Path.home() / ".cache" / "se_bp_index.json"
INDEX_VERSION = 4  # unchanged

Pair = Tuple[str, str]


class IndexData(TypedDict):
    version: int
    workshop_dir: str
    base_data: str
    vanilla_subtypes: List[str]
    pair_to_mods: Dict[str, List[str]]  # "TypeId|SubtypeId" -> [mod ids]
    subtype_to_mods: Dict[str, List[str]]  # SubtypeId -> [mod ids]
    titles: Dict[str, str]


# ---------- small helpers ----------


def is_gzip_file(p: Path) -> bool:
    with p.open("rb") as f:
        return f.read(2) == b"\x1f\x8b"


def open_xml(p: Path) -> io.BufferedReader | gzip.GzipFile:
    return gzip.open(p, "rb") if is_gzip_file(p) else p.open("rb")


def key_pair(t: str, s: str) -> str:
    return f"{t}|{s}"


# ---------- blueprint parsing with counts ----------

_XSI_KEYS: Tuple[str, ...] = (
    "{http://www.w3.org/2001/XMLSchema-instance}type",
    "xsi:type",
    "TypeId",
)


def extract_pair_counts_from_blueprint(bp_path: Path) -> Dict[Pair, int]:
    counts: Dict[Pair, int] = {}
    with open_xml(bp_path) as fh:
        context = ET.iterparse(fh, events=("start", "end"))
        _, root = next(context)
        cur_t: Optional[str] = None
        cur_s: Optional[str] = None
        for event, elem in context:
            tag = elem.tag.split("}")[-1]
            if event == "start" and tag == "MyObjectBuilder_CubeBlock":
                for k in _XSI_KEYS:
                    if k in elem.attrib:
                        cur_t = elem.attrib[k]
                        break
                cur_s = elem.attrib.get("SubtypeName") or elem.attrib.get("Subtype")
            elif event == "end":
                if tag == "TypeId" and not cur_t and (elem.text or "").strip():
                    cur_t = elem.text.strip()
                if (
                    tag in ("SubtypeName", "Subtype")
                    and not cur_s
                    and (elem.text or "").strip()
                ):
                    cur_s = elem.text.strip()
                if tag == "MyObjectBuilder_CubeBlock":
                    t = cur_t or "(unknown-type)"
                    s = (cur_s or "").strip()
                    if s:
                        key = (t, s)
                        counts[key] = counts.get(key, 0) + 1
                    root.clear()
                    cur_t = None
                    cur_s = None
    return counts


# ---------- mod definition scanning (exact Id pairs) ----------


def extract_pairs_from_def_xml(tree: ET.ElementTree) -> Set[Pair]:
    out: Set[Pair] = set()
    root = tree.getroot()
    for id_node in root.iter():
        if id_node.tag.split("}")[-1] != "Id":
            continue
        t = id_node.attrib.get("TypeId")
        s = id_node.attrib.get("SubtypeId")
        if not t or not s:
            t2: Optional[str] = None
            s2: Optional[str] = None
            for child in id_node:
                name = child.tag.split("}")[-1]
                if name == "TypeId" and child.text:
                    t2 = child.text.strip()
                elif name == "SubtypeId" and child.text:
                    s2 = child.text.strip()
            if t2 and s2:
                t, s = t2, s2
        if t and s:
            out.add((t, s))
    return out


RE_ID_ATTR = re.compile(
    r'<\s*Id\b[^>]*\bTypeId\s*=\s*"([^"]+)"[^>]*\bSubtypeId\s*=\s*"([^"]+)"[^>]*/?>',
    re.I | re.S,
)
RE_ID_NESTED = re.compile(
    r"<\s*Id\b[^>]*>.*?<\s*TypeId\s*>\s*([^<]+?)\s*<\s*/\s*TypeId\s*>.*?<\s*SubtypeId\s*>\s*([^<]+?)\s*<\s*/\s*SubtypeId\s*>.*?<\s*/\s*Id\s*>",
    re.I | re.S,
)


def extract_pairs_from_text(txt: str) -> Set[Pair]:
    out: Set[Pair] = set()
    for m in RE_ID_ATTR.finditer(txt):
        out.add((m.group(1).strip(), m.group(2).strip()))
    for m in RE_ID_NESTED.finditer(txt):
        out.add((m.group(1).strip(), m.group(2).strip()))
    return out


def scan_pairs_in_mod(mod_dir: Path) -> Set[Pair]:
    pairs: Set[Pair] = set()
    data_root = mod_dir / "Data"
    roots = [p for p in (data_root if data_root.exists() else mod_dir).rglob("*.sbc")]
    for p in roots:
        try:
            tree = ET.parse(p)
            pairs |= extract_pairs_from_def_xml(tree)
            continue
        except Exception:
            pass
        try:
            txt = p.read_text(encoding="utf-8", errors="ignore")
            pairs |= extract_pairs_from_text(txt)
        except Exception:
            continue
    return pairs


# ---------- titles from mod metadata ----------

_XML_TITLE_TAGS = ("Name", "DisplayName", "FriendlyName", "Title")
RE_JSON_NAME = re.compile(r'"name"\s*:\s*"([^"]+)"', re.I)
RE_XML_ANY_TITLE = re.compile(
    r"<(Name|DisplayName|FriendlyName|Title)>([^<]+)</\1>", re.I
)


def _xml_title(p: Path) -> Optional[str]:
    try:
        tree = ET.parse(p)
        root = tree.getroot()
        for tag in _XML_TITLE_TAGS:
            el = root.find(f".//{tag}")
            if el is not None and (el.text or "").strip():
                return el.text.strip()
    except Exception:
        try:
            txt = p.read_text(encoding="utf-8", errors="ignore")
            m = RE_XML_ANY_TITLE.search(txt)
            if m:
                return m.group(2).strip()
        except Exception:
            pass
    return None


def _sbmi_title(p: Path) -> Optional[str]:
    try:
        txt = p.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return None
    try:
        obj = json.loads(txt)
        if isinstance(obj, dict):
            name = obj.get("name")
            if isinstance(name, str) and name.strip():
                return name.strip()
    except Exception:
        pass
    m = RE_JSON_NAME.search(txt)
    if m:
        return m.group(1).strip()
    return None


def title_from_mod_dir(mod_dir: Path) -> Optional[str]:
    for fname in ("About.sbc", "metadata.mod"):
        p = mod_dir / fname
        if p.exists():
            t = _xml_title(p)
            if t:
                return t
    p = mod_dir / "modinfo.sbmi"
    if p.exists():
        t = _sbmi_title(p)
        if t:
            return t
    return None


# ---------- vanilla & world ----------

RE_WORLD_ID = re.compile(r"<(PublishedFileId|SteamItemId)>(\d+)</\1>", re.I)
RE_WORLD_NAME_NUM = re.compile(r"<Name>(\d+)</Name>", re.I)
RE_ANY_NUM_IN_NAME = re.compile(r"<Name>.*?(\d+).*?</Name>", re.I)


def load_world_ids(world_cfg: Optional[Path]) -> Set[str]:
    ids: Set[str] = set()
    if not world_cfg or not world_cfg.exists():
        return ids
    txt = world_cfg.read_text(encoding="utf-8", errors="ignore")
    ids |= {m.group(2) for m in RE_WORLD_ID.finditer(txt)}
    ids |= {m.group(1) for m in RE_WORLD_NAME_NUM.finditer(txt)}
    ids |= {m.group(1) for m in RE_ANY_NUM_IN_NAME.finditer(txt)}
    return ids


def scan_vanilla(base_data: Path) -> Set[str]:
    subs: Set[str] = set()
    if not base_data.is_dir():
        return subs
    for p in base_data.rglob("*.sbc"):
        try:
            txt = p.read_text(encoding="utf-8", errors="ignore")
        except Exception:
            continue
        for m in re.finditer(
            r"<\s*SubtypeId\s*>\s*([^<]+)\s*<\s*/\s*SubtypeId\s*>", txt, re.I
        ):
            subs.add(m.group(1).strip())
    return subs


# ---------- index (rebuild / load / save) ----------


class IndexBuild:
    def __init__(self) -> None:
        self.vanilla_subs: Set[str] = set()
        self.pair_to_mods: Dict[str, Set[str]] = {}
        self.subtype_to_mods: Dict[str, Set[str]] = {}
        self.titles: Dict[str, str] = {}

    def add_mod(self, mod_id: str, mod_dir: Path) -> None:
        t = title_from_mod_dir(mod_dir)
        if t:
            self.titles[mod_id] = t
        pairs = scan_pairs_in_mod(mod_dir)
        for type_id, sub in pairs:
            self.pair_to_mods.setdefault(key_pair(type_id, sub), set()).add(mod_id)
            self.subtype_to_mods.setdefault(sub, set()).add(mod_id)


def build_index(workshop_dir: Path, base_data: Path, progress: bool) -> IndexData:
    if progress:
        print(f"[rebuild] workshop content: {workshop_dir}", file=sys.stderr)
        print(f"[rebuild] base data:        {base_data}", file=sys.stderr)

    idxb = IndexBuild()
    if progress:
        print("[rebuild] scanning vanilla…", file=sys.stderr)
    idxb.vanilla_subs = scan_vanilla(base_data)
    if progress:
        print(f"[rebuild] vanilla subtypes: {len(idxb.vanilla_subs)}", file=sys.stderr)

    mod_dirs = (
        [d for d in workshop_dir.iterdir() if d.is_dir()]
        if workshop_dir.is_dir()
        else []
    )
    if progress:
        print(f"[rebuild] scanning workshop mods: {len(mod_dirs)}", file=sys.stderr)
    for i, mod in enumerate(mod_dirs, 1):
        idxb.add_mod(mod.name, mod)
        if progress and (i % 50 == 0 or i == len(mod_dirs)):
            print(f"[rebuild] {i}/{len(mod_dirs)} mods", file=sys.stderr)

    if progress:
        print(
            f"[rebuild] indexed exact pairs: {len(idxb.pair_to_mods)}", file=sys.stderr
        )
        print(f"[rebuild] titles loaded:       {len(idxb.titles)}", file=sys.stderr)

    return {
        "version": INDEX_VERSION,
        "workshop_dir": str(workshop_dir),
        "base_data": str(base_data),
        "vanilla_subtypes": sorted(idxb.vanilla_subs),
        "pair_to_mods": {k: sorted(list(v)) for k, v in idxb.pair_to_mods.items()},
        "subtype_to_mods": {
            k: sorted(list(v)) for k, v in idxb.subtype_to_mods.items()
        },
        "titles": idxb.titles,
    }


def save_index(path: Path, idx: IndexData) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    with tmp.open("w", encoding="utf-8") as f:
        json.dump(idx, f, ensure_ascii=False)
    tmp.replace(path)


def load_index(path: Path) -> IndexData:
    with path.open("r", encoding="utf-8") as f:
        raw: Any = json.load(f)
    if not isinstance(raw, dict) or int(raw.get("version", 0)) != INDEX_VERSION:
        raise RuntimeError("Index version mismatch; run with --rebuild")

    def _list_str(x: Any) -> List[str]:
        return [str(v) for v in x] if isinstance(x, list) else []

    def _map_list(x: Any) -> Dict[str, List[str]]:
        out: Dict[str, List[str]] = {}
        if isinstance(x, dict):
            for k, v in x.items():
                if isinstance(k, str) and isinstance(v, list):
                    out[k] = [str(i) for i in v]
        return out

    idx: IndexData = {
        "version": int(raw["version"]),
        "workshop_dir": str(raw.get("workshop_dir", "")),
        "base_data": str(raw.get("base_data", "")),
        "vanilla_subtypes": _list_str(raw.get("vanilla_subtypes")),
        "pair_to_mods": _map_list(raw.get("pair_to_mods")),
        "subtype_to_mods": _map_list(raw.get("subtype_to_mods")),
        "titles": {str(k): str(v) for k, v in (raw.get("titles") or {}).items()},
    }
    return idx


# ---------- table ----------


def print_table(rows: List[Tuple[str, str, int, str, str, str]]) -> None:
    headers = ("TypeId", "Subtype", "Count", "ModTitle", "Status", "WorkshopURL")
    data: List[Tuple[str, str, str, str, str, str]] = [headers] + [
        (r[0], r[1], str(r[2]), r[3], r[4], r[5]) for r in rows
    ]
    widths = [max(len(str(row[i])) for row in data) for i in range(len(headers))]
    fmt = "  ".join(f"{{:{w}}}" for w in widths)
    sep = "  ".join("-" * w for w in widths)
    print(fmt.format(*headers))
    print(sep)
    for r in rows:
        print(fmt.format(r[0], r[1], r[2], r[3], r[4], r[5]))


# ---------- main ----------


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Space Engineers blueprint → mod resolver (exact pair index + counts)."
    )
    ap.add_argument("bp_sbc", type=Path, nargs="?", help="bp.sbc (gz or plain)")
    ap.add_argument("-w", "--world", type=Path, help="Sandbox.sbc / Sandbox_config.sbc")
    ap.add_argument(
        "-m",
        "--missing-only",
        action="store_true",
        help="With -w, show only mods missing from the world",
    )
    ap.add_argument("--vanilla", action="store_true", help="Include vanilla blocks")
    ap.add_argument("--rebuild", action="store_true", help="Rebuild index and exit")
    ap.add_argument(
        "--index", type=Path, default=DEFAULT_INDEX_PATH, help="Index file path"
    )
    ap.add_argument(
        "-W",
        "--workshop-dir",
        type=Path,
        default=DEFAULT_WORKSHOP_CONTENT,
        help="…/workshop/content/244850 (rebuild only)",
    )
    ap.add_argument(
        "-B",
        "--base-data",
        type=Path,
        default=DEFAULT_BASE_DATA,
        help="…/SpaceEngineers/Content/Data (rebuild only)",
    )
    args = ap.parse_args()

    if args.rebuild:
        print("[rebuild] starting…", file=sys.stderr)
        idx = build_index(args.workshop_dir, args.base_data, progress=True)
        save_index(args.index, idx)
        print(f"[rebuild] wrote index: {args.index}", file=sys.stderr)
        return 0

    if args.bp_sbc is None:
        print("bp.sbc is required (or run with --rebuild).", file=sys.stderr)
        return 2

    if not args.index.exists():
        print(f"Index not found: {args.index}\nRun with --rebuild.", file=sys.stderr)
        return 2

    try:
        idx = load_index(args.index)
    except Exception as e:
        print(f"Failed to load index: {e}", file=sys.stderr)
        return 2

    pair_counts = extract_pair_counts_from_blueprint(args.bp_sbc)
    world_ids = load_world_ids(args.world)

    vanilla_subs: Set[str] = set(idx["vanilla_subtypes"])
    pair_to_mods: Dict[str, List[str]] = idx["pair_to_mods"]
    subtype_to_mods: Dict[str, List[str]] = idx["subtype_to_mods"]
    titles: Dict[str, str] = idx["titles"]

    rows: List[Tuple[str, str, int, str, str, str]] = []

    for type_id, subtype in sorted(pair_counts.keys()):
        count = pair_counts[(type_id, subtype)]

        if subtype in vanilla_subs:
            if args.vanilla and not args.missing_only:
                rows.append((type_id, subtype, count, "Vanilla", "VANILLA", "-"))
            continue

        mods = pair_to_mods.get(key_pair(type_id, subtype))
        if not mods:
            mods = subtype_to_mods.get(subtype, [])

        if not mods:
            rows.append((type_id, subtype, count, "-", "MISSING", "-"))
            continue

        mods_set = set(mods)
        in_world = mods_set.intersection(world_ids) if args.world else set()
        if args.missing_only and in_world:
            continue

        chosen_title = (
            "(multiple)"
            if len(mods_set) > 1
            else titles.get(next(iter(mods_set), ""), "(unknown title)")
        )
        if len(in_world) == 1:
            wid = next(iter(in_world))
            chosen_title = titles.get(wid, chosen_title)

        status = "IN_WORLD" if in_world else "MISSING"
        all_ids = sorted(mods_set)
        links = [
            f"https://steamcommunity.com/sharedfiles/filedetails/?id={wid}"
            for wid in all_ids[:3]
        ]
        if len(all_ids) > 3:
            links.append(f"...(+{len(all_ids) - 3})")

        rows.append((type_id, subtype, count, chosen_title, status, ", ".join(links)))

    print_table(rows)
    return 0


if __name__ == "__main__":
    sys.exit(main())
