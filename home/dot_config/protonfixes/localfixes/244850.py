"""Game fix for Space Engineers"""

import io
import json
import urllib.request
import zipfile
from pathlib import Path
from protonfixes import util


def _install_pulsar(game_path: Path) -> None:
    pulsar_dir = game_path / "Pulsar"
    if (pulsar_dir / "Legacy.exe").exists():
        return

    try:
        api_url = "https://api.github.com/repos/SpaceGT/Pulsar/releases/latest"
        with urllib.request.urlopen(api_url) as response:
            release = json.loads(response.read())

        zip_url = next(
            asset["browser_download_url"]
            for asset in release["assets"]
            if asset["name"].startswith("Pulsar-") and asset["name"].endswith(".zip")
        )

        with urllib.request.urlopen(zip_url) as response:
            zip_data = response.read()

        pulsar_dir.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(io.BytesIO(zip_data)) as zf:
            zf.extractall(pulsar_dir)
    except Exception:
        pass


def main() -> None:
    util.set_environment("MANGOHUD", "1")

    game_path = Path(util.get_game_install_path())
    _install_pulsar(game_path)

    util.protontricks("xaudio29")
    util.protontricks("dotnet48")
    util.protontricks("vcrun2019")
    util.protontricks("dotnetdesktop10")

    base_attribute = "<runtime>"
    game_opts = """
    <loadFromRemoteSources enabled="true" />
    <gcServer enabled = "true" />
    <gcConcurrent enabled="false" />
"""

    util.set_xml_options(base_attribute, game_opts, "Bin64/SpaceEngineers.exe.config")
    if (game_path / "Pulsar" / "Legacy.exe").exists():
        util.set_xml_options(base_attribute, game_opts, "Pulsar/Legacy.exe.config")
        # Replace SE.exe with Pulsar's launcher in argv, but Pulsar still needs
        # to locate Bin64. Its FromSteamArgs scan can't fire here (we just
        # overwrote the SE.exe arg) and FromSteamFiles/FromRegistry skip under
        # Proton, so hand it -bin64 explicitly. Pulsar's TryConvertUnix prepends
        # Z: to unix paths.
        util.replace_command(r"Bin64.SpaceEngineers\.exe", r"Pulsar\\Interim.exe")
        util.append_argument("-bin64")
        util.append_argument(str(game_path / "Bin64"))

    external = Path.home() / "GameData" / "SpaceEngineers"
    internal = (
        util.protonprefix() / "drive_c/users/steamuser/AppData/Roaming/SpaceEngineers"
    )

    if not internal.is_symlink():
        external.mkdir(parents=True, exist_ok=True)
        internal.symlink_to(external)
