"""Game fix for Space Engineers"""

from pathlib import Path
from protonfixes import util


def main() -> None:
    util.protontricks('xaudio29')
    util.protontricks('dotnet48')
    util.protontricks('vcrun2019')

    base_attribute = '<runtime>'
    game_opts = """
    <loadFromRemoteSources enabled="true" />
    <gcServer enabled = "true" />
    <gcConcurrent enabled="false" />
"""

    util.set_xml_options(base_attribute, game_opts, 'Bin64/SpaceEngineers.exe.config')
    util.set_xml_options(base_attribute, game_opts, 'Pulsar/Legacy.exe.config')

    external = Path.home() / 'GameData' / 'SpaceEngineers'
    internal = util.protonprefix() / 'drive_c/users/steamuser/AppData/Roaming/SpaceEngineers'

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
