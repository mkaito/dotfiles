#!/usr/bin/env python3
"""
Sort multi-disc ROM files into .m3u folders following No-Intro naming convention.
"""

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path

ROM_EXTENSIONS = {".chd", ".iso", ".bin", ".cue", ".img"}
DISC_PATTERN = re.compile(r"\s*\(Disc\s*(\d+)\)", re.IGNORECASE)

STANDARD_TAGS = [
    r'\(Rev\s+[\dA-Z]+\)',
    r'\(Ver\s+[\d.]+\)',
    r'\(v[\d.]+\)',
    r'\(Beta\)',
    r'\(Demo\)',
    r'\(Proto\)',
    r'\(Sample\)',
    r'\(Promo\)',
    r'\(Unl\)',
    r'\(Alt\)',
    r'\(Kiosk\)',
    r'\(Not For Resale\)',
    r'\(Competition\)',
    r'\(Prize\)',
    r'\(Reprint\)',
    r'\(Program\)',
]

STANDARD_TAG_PATTERN = re.compile('|'.join(STANDARD_TAGS), re.IGNORECASE)


def extract_disc_info(filename):
    match = DISC_PATTERN.search(filename)
    if not match:
        return None
    disc_num = int(match.group(1))
    base_identity = filename[:match.start()].rstrip()
    
    after_disc = filename[match.end():]
    ext_match = re.search(r'\.\w+$', after_disc)
    tags_portion = after_disc[:ext_match.start()] if ext_match else after_disc
    
    standard_tags = []
    for tag_match in STANDARD_TAG_PATTERN.finditer(tags_portion):
        standard_tags.append(tag_match.group(0))
    
    if standard_tags:
        identity = base_identity + " " + " ".join(standard_tags)
    else:
        identity = base_identity
    
    return disc_num, identity


def scan_disc_files(directory):
    directory = Path(directory)
    groups = defaultdict(list)
    
    for file_path in directory.rglob("*"):
        if not file_path.is_file() or file_path.suffix.lower() not in ROM_EXTENSIONS:
            continue
        
        disc_info = extract_disc_info(file_path.name)
        if not disc_info:
            continue
        
        disc_num, identity = disc_info
        parent_dir = file_path.parent if file_path.parent != directory else None
        groups[identity].append((disc_num, file_path, parent_dir))
    
    return groups


def get_correct_folder(identity, directory):
    identity_path = Path(identity)
    folder_name = identity_path.stem + ".m3u"
    return directory / folder_name


def verify_m3u_content(m3u_path, expected_filenames):
    if not m3u_path.exists():
        return False
    
    try:
        with m3u_path.open("r", encoding="utf-8") as f:
            lines = [line.rstrip("\n\r") for line in f.readlines() if line.strip()]
        
        if lines != expected_filenames:
            return False
        
        with m3u_path.open("rb") as f:
            f.seek(-1, 2)
            return f.read(1) == b"\n"
    except OSError:
        return False


def write_m3u(m3u_path, filenames):
    with m3u_path.open("w", encoding="utf-8") as f:
        for filename in filenames:
            f.write(filename + "\n")


def ensure_correct_state(identity, disc_files, directory, dry_run=False, verbose=False):
    if len(disc_files) < 2:
        return False
    
    disc_files.sort(key=lambda x: x[0])
    
    correct_folder = get_correct_folder(identity, directory)
    correct_m3u = correct_folder / (Path(identity).stem + ".m3u")
    
    check_dir = directory
    while check_dir != check_dir.parent:
        if check_dir.suffix == ".m3u":
            print(f"Error: Cannot create folders inside .m3u folder: {check_dir}", file=sys.stderr)
            return False
        check_dir = check_dir.parent
    
    existing_folders = {parent for _, _, parent in disc_files if parent is not None}
    
    if correct_folder.exists():
        target_folder = correct_folder
    elif existing_folders:
        target_folder = next(iter(existing_folders))
    else:
        target_folder = correct_folder
    
    files_to_move = [(d, p) for d, p, parent in disc_files if parent != correct_folder and p.exists()]
    needs_rename = target_folder != correct_folder and target_folder.exists()
    needs_moves = len(files_to_move) > 0
    
    if dry_run:
        if needs_rename:
            print(f"[DRY RUN] Would rename: {target_folder.name} -> {correct_folder.name}")
        if needs_moves:
            print(f"[DRY RUN] Would move {len(files_to_move)} file(s) to {correct_folder.name}/")
        if not correct_folder.exists() and not needs_rename:
            print(f"[DRY RUN] Would create: {correct_folder.name}")
        print(f"[DRY RUN] Would verify/update: {correct_folder.name}/{correct_m3u.name}")
        return True
    
    if not target_folder.exists():
        target_folder.mkdir(parents=True, exist_ok=False)
    
    for disc_num, file_path in files_to_move:
        target = target_folder / file_path.name
        if target == file_path:
            continue
        if target.exists():
            print(f"Error: Target exists: {target}", file=sys.stderr)
            return False
        if not file_path.exists():
            if target.exists():
                continue
            print(f"Warning: Source missing: {file_path}", file=sys.stderr)
            continue
        file_path.rename(target)
    
    if needs_rename:
        target_folder.rename(correct_folder)
        for m3u_file in correct_folder.glob("*.m3u"):
            if m3u_file.name != correct_m3u.name:
                m3u_file.rename(correct_m3u)
    
    if not correct_folder.exists():
        correct_folder.mkdir(parents=True, exist_ok=False)
    
    actual_filenames = sorted([p.name for _, p, _ in disc_files], key=lambda n: int(DISC_PATTERN.search(n).group(1)))
    
    if not verify_m3u_content(correct_m3u, actual_filenames):
        write_m3u(correct_m3u, actual_filenames)
        if needs_rename or needs_moves:
            action = "Fixed" if (needs_rename or correct_m3u.exists()) else "Created"
            print(f"{action}: {correct_folder.name} ({len(disc_files)} disc(s))")
        return True
    
    if needs_rename or needs_moves:
        print(f"Fixed: {correct_folder.name} ({len(disc_files)} disc(s))")
        return True
    
    return True


def process_platform_directory(platform_dir, dry_run=False, verbose=False):
    """Process a single platform directory for multi-disc ROMs."""
    disc_groups = scan_disc_files(platform_dir)
    
    if not disc_groups:
        return 0, 0
    
    processed = 0
    skipped = 0
    
    for identity, disc_files in disc_groups.items():
        if verbose:
            print(f"\nProcessing: {Path(identity).stem} ({len(disc_files)} disc(s))")
        
        if ensure_correct_state(identity, disc_files, platform_dir, dry_run, verbose):
            if len(disc_files) >= 2:
                processed += 1
        elif len(disc_files) >= 2:
            skipped += 1
    
    return processed, skipped


def find_platform_directories(root_dir):
    """Find platform directories that contain multi-disc ROM files."""
    root_dir = Path(root_dir)
    platform_dirs = set()
    
    for file_path in root_dir.rglob("*"):
        if not file_path.is_file() or file_path.suffix.lower() not in ROM_EXTENSIONS:
            continue
        
        disc_info = extract_disc_info(file_path.name)
        if not disc_info:
            continue
        
        # Determine the platform directory: immediate subdirectory of root, or root itself
        rel_path = file_path.relative_to(root_dir)
        if len(rel_path.parts) == 1:
            # File is directly in root
            platform_dir = root_dir
        else:
            # File is in a subdirectory - use the first subdirectory as platform dir
            platform_dir = root_dir / rel_path.parts[0]
        
        platform_dirs.add(platform_dir)
    
    return sorted(platform_dirs)


def main():
    parser = argparse.ArgumentParser(description="Sort multi-disc ROM files into .m3u folders")
    parser.add_argument("directory", nargs="?", default=".", help="Directory to process")
    parser.add_argument("--dry-run", action="store_true", help="Preview changes")
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")
    
    args = parser.parse_args()
    directory = Path(args.directory).resolve()
    
    if not directory.is_dir():
        print(f"Error: Not a directory: {directory}", file=sys.stderr)
        sys.exit(1)
    
    # Find platform directories that contain multi-disc ROM files
    platform_dirs = find_platform_directories(directory)
    
    if not platform_dirs:
        if args.verbose:
            print("No multi-disc ROM files found.")
        return
    
    total_processed = 0
    total_skipped = 0
    
    for platform_dir in platform_dirs:
        if args.verbose and len(platform_dirs) > 1:
            print(f"\n=== Processing platform: {platform_dir.name} ===")
        
        processed, skipped = process_platform_directory(platform_dir, args.dry_run, args.verbose)
        total_processed += processed
        total_skipped += skipped
    
    if args.verbose or args.dry_run:
        print(f"\nSummary: {total_processed} processed, {total_skipped} skipped")


if __name__ == "__main__":
    main()
