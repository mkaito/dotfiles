from pathlib import Path
from protonfixes import util


def main():
    external = Path.home() / 'GameData' / 'SpaceEngineers2'
    internal = util.protonprefix() / 'drive_c/users/steamuser/AppData/Roaming/SpaceEngineers2'

    if not internal.is_symlink():
        external.mkdir(parents=True, exist_ok=True)
        internal.symlink_to(external)
