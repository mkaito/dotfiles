import os
from pathlib import Path
from protonfixes import util


def main():
    external = Path.home() / 'GameData' / 'SpaceEngineers2'
    internal = util.protonprefix() / 'drive_c/users/steamuser/AppData/Roaming/SpaceEngineers2'

    if internal.is_symlink():
        return

    external.mkdir(parents=True, exist_ok=True)

    if internal.is_dir():
        conflicts = [item.name for item in internal.iterdir() if (external / item.name).exists()]
        if conflicts:
            raise FileExistsError(f'Refusing to overwrite existing files in {external}: {conflicts}')
        for item in internal.iterdir():
            item.rename(external / item.name)
        internal.rmdir()

    internal.symlink_to(external)
