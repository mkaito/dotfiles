# Copyright 2026 Christian Höppner
# Distributed under the terms of the GNU General Public License v2

# Cargo crate deps are vendored offline rather than enumerated via the cargo
# eclass CRATES= list - that list runs ~300 entries long for this project and
# is impractical to maintain by hand. The vendor tarball is produced by
# files/make-vendor.sh and stored in FILESDIR. Bump it whenever PV bumps.

EAPI=8

RUST_MIN_VER="1.88.0"

inherit cargo desktop toolchain-funcs xdg

DESCRIPTION="Cross-platform native runtime for Path of Building (PoE1/PoE2)"
HOMEPAGE="https://github.com/meehl/rusty-path-of-building"
SRC_URI="https://github.com/meehl/rusty-path-of-building/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"

# rusty-path-of-building is MIT; the vendored crates contribute a wide mix of
# permissive licences. Listing the union of every crate licence would be
# cargo-license output noise - patch this when ultraviolet over a release.
LICENSE="MIT Apache-2.0 BSD MPL-2.0 ZLIB || ( Apache-2.0 MIT )"
SLOT="0"
KEYWORDS="-* ~amd64"
RESTRICT="mirror"

# Build-time C linkage:
#   - mlua[luajit] links system LuaJIT (Cargo.toml: features = ["luajit"])
#   - reqwest pulls openssl via native-tls on Linux by default
#   - wgpu/winit use Wayland + X11 + libxkbcommon at runtime via dlopen,
#     but their *-sys crates still want headers/pkg-config at build time.
DEPEND="
	dev-lang/luajit:=
	dev-libs/openssl:=
	dev-libs/wayland
	media-libs/fontconfig
	virtual/zlib
	x11-libs/libxkbcommon
	x11-libs/libX11
	x11-libs/libXcursor
	x11-libs/libXi
	x11-libs/libXrandr
"

# Runtime additions:
#   - vulkan-loader: wgpu picks the Vulkan backend on Linux.
#   - PoB's Lua code requires lua-curl, lua-utf8 and luasocket built for
#     LuaJIT. The lzip Lua C module ships inside this ebuild.
RDEPEND="
	${DEPEND}
	media-libs/vulkan-loader
	dev-lua/lua-curl[lua_targets_luajit(-)]
	dev-lua/lua-utf8[lua_targets_luajit(-)]
	dev-lua/luasocket[lua_targets_luajit(-)]
"

BDEPEND="
	virtual/pkgconfig
"

src_unpack() {
	default

	if [[ ! -f ${FILESDIR}/${P}-vendor.tar.xz ]]; then
		eerror "Missing vendored crates tarball:"
		eerror "    ${FILESDIR}/${P}-vendor.tar.xz"
		eerror
		eerror "Generate it with:"
		eerror "    ${FILESDIR}/make-vendor.sh ${PV}"
		die "vendor tarball missing"
	fi

	# Drop the cargo-vendored crate dirs into the eclass's expected vendor
	# location, then let cargo_gen_config write the offline config.toml.
	mkdir -p "${ECARGO_VENDOR}" || die
	tar -xf "${FILESDIR}/${P}-vendor.tar.xz" \
		--strip-components=1 -C "${ECARGO_VENDOR}" \
		|| die "vendor unpack failed"

	cargo_gen_config
}

src_compile() {
	cargo_src_compile

	# Bundled lzip Lua C module - the only Lua dep PoB needs that has no
	# Gentoo package. Builds against system LuaJIT + zlib.
	emake -C lua/libs/lzip LUA_IMPL=luajit CC="$(tc-getCC)" CXX="$(tc-getCXX)"
}

src_install() {
	cargo_src_install

	# LuaJIT shares its cmod dir with lua5.1 in Gentoo's lua eclass world.
	exeinto "/usr/$(get_libdir)/lua/5.1"
	doexe lua/libs/lzip/lzip.so

	# Thin per-game wrappers so `pob1` / `pob2` work from PATH and from the
	# .desktop entries (Wayland app_id is set by the binary itself).
	local g
	for g in 1 2 ; do
		cat > "${T}/pob${g}" <<-EOF || die
			#!/bin/sh
			exec ${EPREFIX}/usr/bin/${PN} poe${g} "\$@"
		EOF
		chmod +x "${T}/pob${g}" || die
		dobin "${T}/pob${g}"
	done

	newicon -s 256 assets/icon.png "${PN}.png"

	# Two .desktop entries. Their filenames must match RustyPoB's Wayland
	# app_id (rusty-path-of-building-1 / -2) for icons to bind correctly.
	local de
	for de in 1 2 ; do
		cat > "${T}/${PN}-${de}.desktop" <<-EOF || die
			[Desktop Entry]
			Type=Application
			Name=Path of Building $([[ ${de} == 2 ]] && echo 2)
			GenericName=Path of Exile $([[ ${de} == 2 ]] && echo 2) build planner
			Exec=pob${de}
			Icon=${PN}
			Categories=Game;
			StartupNotify=true
			StartupWMClass=${PN}-${de}
		EOF
	done
	domenu "${T}/${PN}-1.desktop" "${T}/${PN}-2.desktop"
}

pkg_postinst() {
	xdg_pkg_postinst

	elog "Launch with:"
	elog "    pob1   # Path of Building (PoE1)"
	elog "    pob2   # Path of Building 2"
	elog "    ${PN} poe{1,2}   # equivalent direct invocation"
	elog
	elog "PoB's Lua code is fetched on first run into:"
	elog "    ~/.local/share/RustyPathOfBuilding{1,2}/"
	elog
	elog "If automatic updates wedge, delete rpob.version and the Update/"
	elog "directory inside that path to force a full re-download."
}
