# Copyright 2026 Christian Höppner
# Distributed under the terms of the GNU General Public License v2

# Binary repack of the official MagnetarForLinux .7z release from
# viktor-ferenczi/Magnetar. Magnetar is a plugin/mod loader for the Space
# Engineers Dedicated Server (SE1) - a hard fork of Pulsar that runs the
# headless DS natively on Linux on .NET 10 via se-dotnet-compat and
# se-linux-compat.
#
# Plugins are not packaged - Magnetar fetches them from PluginHub on first run.

EAPI=8

DESCRIPTION="Magnetar plugin/mod loader for the Space Engineers Dedicated Server"
HOMEPAGE="https://github.com/viktor-ferenczi/Magnetar"
SRC_URI="https://github.com/viktor-ferenczi/Magnetar/releases/download/v${PV}/MagnetarForLinux-${PV}.7z"

# The .7z packs a top-level MagnetarForLinux/ dir (version lives only in the
# asset filename).
S="${WORKDIR}/MagnetarForLinux"

# Magnetar is MIT; the bundled Steam/EOS SDK blobs are all-rights-reserved,
# redistributed from upstream's public release.
LICENSE="MIT all-rights-reserved"
SLOT="0"
KEYWORDS="-* ~amd64"
RESTRICT="bindist mirror strip"
QA_PREBUILT="opt/magnetar/Bin/*"

# Framework-dependent .NET 10 app. Gentoo has no separate dotnet-runtime-bin,
# so RDEPEND the SDK (it ships the runtime too). Steamworks.NET, libsteam_api,
# the EOS SDK and the VRage/Havok/RecastDetour wrappers all live in the
# bundle's Bin/ next to the apphost and are resolved from there.
RDEPEND="dev-dotnet/dotnet-sdk-bin:10.0"
BDEPEND="app-arch/7zip"

src_unpack() {
	7z x -y "${DISTDIR}/MagnetarForLinux-${PV}.7z" > /dev/null || die "7z extract failed"
}

src_prepare() {
	default

	# The MagnetarInterim shell launcher self-locates PKG_DIR via "dirname $0",
	# which breaks when invoked through the /usr/bin/magnetar symlink ($0 then
	# points at /usr/bin/magnetar so the Bin/MagnetarInterim apphost lookup
	# fails). readlink -f resolves the symlink before dirname so the launcher
	# works regardless of how it was invoked.
	sed -i -e \
		's|PKG_DIR="\$(cd "\$(dirname "\$0")" && pwd)"|PKG_DIR="$(cd "$(dirname "$(readlink -f "$0")")" \&\& pwd)"|' \
		"${S}/Magnetar/MagnetarInterim" || die "patching MagnetarInterim PKG_DIR failed"
	grep -q 'readlink -f "$0"' "${S}/Magnetar/MagnetarInterim" \
		|| die "MagnetarInterim PKG_DIR patch did not apply"
}

src_install() {
	# Bundle tree -> /opt/magnetar/. Use cp -ar so the apphost ELF keeps +x.
	dodir /opt/magnetar
	cp -ar "${S}/Magnetar/Bin" "${ED}/opt/magnetar/" || die "failed copying Bin tree"

	# Shell launcher at /opt/magnetar/MagnetarInterim; src_prepare patched it to
	# resolve the symlink-via-/usr/bin/magnetar invocation correctly.
	exeinto /opt/magnetar
	doexe "${S}/Magnetar/MagnetarInterim"

	# PATH access via /usr/bin/magnetar -> /opt/magnetar/MagnetarInterim
	dosym -r /opt/magnetar/MagnetarInterim /usr/bin/magnetar
}

pkg_postinst() {
	elog "Run the dedicated server through Magnetar in place of"
	elog "SpaceEngineersDedicated, e.g.:"
	elog "    magnetar -console"
	elog ""
	elog "Requires the Space Engineers Dedicated Server installed (Steam/steamcmd) at:"
	elog "    ~/.steam/steam/steamapps/common/SpaceEngineersDedicatedServer/DedicatedServer64"
	elog "Override the DS path with the DS64 env var or '-ds64 /path/to/DedicatedServer64'."
	elog ""
	elog "On first run Magnetar fetches plugins from PluginHub (outbound HTTPS to github.com)."
	elog ""
	elog "User state lives under ~/.config/Magnetar/ (config.xml, Sources/, Local/,"
	elog "Profiles/) and survives uninstall."
}
