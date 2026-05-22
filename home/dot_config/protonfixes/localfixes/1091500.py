"""Game fix for Cyberpunk 2077"""

from protonfixes import util


def main() -> None:
    # CP2077's RED4ext + CET ASI plugins depend on MS C++ runtime (MSVCP140 →
    # ucrtbase chain). Proton's prefix init replaces native ucrtbase with the
    # Wine builtin on (re)install, which leaves MSVCP140 calling into a stub
    # CRT and crashes the loader with an access violation. Reinstalling
    # vcrun2022 restores native MS CRT DLLs.
    util.protontricks('vcrun2022')
