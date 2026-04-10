"""Game fix for Space Engineers"""

import io
import json
import urllib.request
import zipfile
from pathlib import Path
from protonfixes import util


def _install_pulsar(game_path: Path) -> None:
    pulsar_dir = game_path / 'Pulsar'
    if (pulsar_dir / 'Legacy.exe').exists():
        return

    api_url = 'https://api.github.com/repos/SpaceGT/Pulsar/releases/latest'
    with urllib.request.urlopen(api_url) as response:
        release = json.loads(response.read())

    zip_url = next(
        asset['browser_download_url']
        for asset in release['assets']
        if asset['name'].startswith('Pulsar-') and asset['name'].endswith('.zip')
    )

    with urllib.request.urlopen(zip_url) as response:
        zip_data = response.read()

    pulsar_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(io.BytesIO(zip_data)) as zf:
        zf.extractall(pulsar_dir)


def main() -> None:
    util.set_environment('MANGOHUD', '1')

    game_path = Path(util.get_game_install_path())
    _install_pulsar(game_path)

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

    util.replace_command(r'Bin64.SpaceEngineers\.exe', r'Pulsar\\Legacy.exe')

    external = Path.home() / 'GameData' / 'SpaceEngineers'
    internal = util.protonprefix() / 'drive_c/users/steamuser/AppData/Roaming/SpaceEngineers'

    if not internal.is_symlink():
        external.mkdir(parents=True, exist_ok=True)
        internal.symlink_to(external)
