"""Game fix for Path of Exile 2"""

from pathlib import Path
from protonfixes import util


def main():
    external = Path.home() / 'GameData' / 'PoE2'
    internal = (
        util.protonprefix()
        / 'drive_c/users/steamuser/Documents/My Games/Path of Exile 2'
    )

    if not internal.is_symlink():
        external.mkdir(parents=True, exist_ok=True)
        internal.symlink_to(external)
