# Copyright 2026 Christian Höppner
# Distributed under the terms of the GNU General Public License v2

# Binary repack of the official PulsarForLinux-Native .7z release from
# viktor-ferenczi/se-linux-compat. Pulsar is a third-party launcher for
# Space Engineers; the 'linux' branch of SpaceGT/Pulsar adds runtime
# patches so SE runs natively on Linux instead of via Proton.
#
# Plugins (se-linux-compat, se-dotnet-compat) are not packaged - Pulsar
# fetches them from PluginHub on first launch.

EAPI=8

inherit desktop xdg

# Upstream releases viktor-ferenczi/se-linux-compat X.Y.Z bundle a Pulsar build
# stamped with date + 8-char SHA. PV here is the bundle date; bump the constants
# below in lockstep with each upstream release. Use -rN if a same-day rebuild
# needs to land.
UPSTREAM_RELEASE_TAG="1.0.7"
UPSTREAM_PULSAR_SHA="13a4f2f7"
PULSAR_NATIVE_7Z="PulsarForLinux-Native.${PV}.${UPSTREAM_PULSAR_SHA}.7z"

DESCRIPTION="Pulsar launcher for Space Engineers (native Linux build)"
HOMEPAGE="
	https://github.com/SpaceGT/Pulsar/tree/linux
	https://github.com/viktor-ferenczi/se-linux-compat
"
SRC_URI="https://github.com/viktor-ferenczi/se-linux-compat/releases/download/${UPSTREAM_RELEASE_TAG}/${PULSAR_NATIVE_7Z}"

# Pulsar itself is MIT; the bundled Valve/Epic SDK blobs are
# all-rights-reserved, redistributed from upstream's public release.
S="${WORKDIR}/PulsarForLinux-Native"

LICENSE="MIT all-rights-reserved"
SLOT="0"
KEYWORDS="-* ~amd64"
RESTRICT="bindist mirror strip"
QA_PREBUILT="opt/pulsar/Bin/* opt/pulsar/Interim"

# Pulsar is a framework-dependent .NET 10 app. Gentoo has no separate
# dev-dotnet/dotnet-runtime-bin, so RDEPEND the SDK (it ships the
# runtime too). FFmpeg, DXVK, EOS, Steam SDK, Steamworks.NET and the
# VRage/Havok wrappers all live in the bundle's Pulsar/Bin/ next to
# the apphost and are resolved by .NET's DllImport resolver from there.
# SDL3, OpenAL, Opus and the Vulkan loader are not bundled and come
# from Portage. X11/XCB are transitive via SDL3.
RDEPEND="
	dev-dotnet/dotnet-sdk-bin:10.0
	media-libs/libsdl3
	media-libs/sdl3-ttf
	media-libs/openal
	media-libs/opus
	media-libs/vulkan-loader
"
BDEPEND="app-arch/7zip"

src_unpack() {
	7z x -y "${DISTDIR}/${PULSAR_NATIVE_7Z}" > /dev/null || die "7z extract failed"
}

src_prepare() {
	default

	# The Interim shell launcher self-locates PKG_DIR via "dirname $0", which
	# breaks when invoked through the /usr/bin/pulsar symlink ($0 then points
	# at /usr/bin/pulsar so PKG_DIR resolves to /usr/bin and the apphost lookup
	# fails). readlink -f resolves the symlink before dirname so the launcher
	# works regardless of how it was invoked.
	sed -i -e \
		's|PKG_DIR="\$(cd "\$(dirname "\$0")" && pwd)"|PKG_DIR="$(cd "$(dirname "$(readlink -f "$0")")" \&\& pwd)"|' \
		"${S}/Pulsar/Interim" || die "patching Interim PKG_DIR failed"
	grep -q 'readlink -f "$0"' "${S}/Pulsar/Interim" \
		|| die "Interim PKG_DIR patch did not apply"
}

src_install() {
	# Bundle tree -> /opt/pulsar/. Use cp -ar so the apphost ELF keeps +x.
	dodir /opt/pulsar
	cp -ar "${S}/Pulsar/Bin" "${ED}/opt/pulsar/" || die "failed copying Bin tree"

	# Shell launcher at /opt/pulsar/Interim; src_prepare patched it to resolve
	# the symlink-via-/usr/bin/pulsar invocation correctly.
	exeinto /opt/pulsar
	doexe "${S}/Pulsar/Interim"

	# PATH access via /usr/bin/pulsar -> /opt/pulsar/Interim
	dosym -r /opt/pulsar/Interim /usr/bin/pulsar

	# Hicolor icons (sizes shipped by upstream: 16/24/32/48/64/96/128/256)
	local size
	for size in 16 24 32 48 64 96 128 256 ; do
		local src="${S}/icons/${size}/pulsar.png"
		[[ -f ${src} ]] && doicon -s ${size} "${src}"
	done

	# Desktop entry. Matches upstream install.sh's "-keepintro" so the
	# menu shortcut keeps the launcher's previous default behaviour.
	# Comment is auto-populated from DESCRIPTION by the eclass.
	make_desktop_entry "/usr/bin/pulsar -keepintro" "Pulsar" "pulsar" "Game;" \
		"GenericName=Space Engineers Launcher (Native Linux)
Keywords=SpaceEngineers;Pulsar;Game;"

	dodoc "${S}/README.txt"
}

pkg_postinst() {
	xdg_pkg_postinst

	elog "Run 'pulsar' or pick Pulsar from your menu (Games)."
	elog ""
	elog "Space Engineers must be installed via Steam at:"
	elog "    ~/.steam/steam/steamapps/common/SpaceEngineers/"
	elog "Override by setting SPACE_ENGINEERS_ROOT before running."
	elog ""
	elog "On first run Pulsar fetches the se-linux-compat and se-dotnet-compat"
	elog "plugins from PluginHub - requires outbound HTTPS to github.com."
	elog ""
	elog "User state lives under:"
	elog "    ~/.config/Pulsar/        sources.xml, Local plugins, profiles"
	elog "    ~/.local/share/Pulsar/   cached plugin builds (safe to delete)"
	elog "Both survive uninstall of the ebuild."
}
