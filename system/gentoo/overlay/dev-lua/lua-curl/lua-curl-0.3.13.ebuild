# Copyright 2026 Christian Höppner
# Distributed under the terms of the GNU General Public License v2

EAPI=8

LUA_COMPAT=( lua5-{1..4} luajit )

inherit lua toolchain-funcs

DESCRIPTION="Lua binding to libcurl (Lua-cURLv3)"
HOMEPAGE="https://github.com/Lua-cURL/Lua-cURLv3"
SRC_URI="https://github.com/Lua-cURL/Lua-cURLv3/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
S="${WORKDIR}/Lua-cURLv3-${PV}"

LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64"
REQUIRED_USE="${LUA_REQUIRED_USE}"
RESTRICT="test"

RDEPEND="
	${LUA_DEPS}
	net-misc/curl
"
DEPEND="${RDEPEND}"
BDEPEND="virtual/pkgconfig"

src_prepare() {
	default
	lua_copy_sources
}

lua_src_compile() {
	pushd "${BUILD_DIR}" > /dev/null || die

	# Force the Makefile's "no pkg-config" branch and override its hard-coded
	# defaults so the build picks up the active Lua impl and Gentoo paths.
	# Lua C modules resolve Lua symbols from the host at dlopen time, so
	# nothing here needs to link against liblua/libluajit.
	emake \
		CC="$(tc-getCC)" \
		LUA_VERSION=skip \
		LUA_INC="$(lua_get_include_dir)" \
		LUA_CMOD="$(lua_get_cmod_dir)" \
		LUA_LMOD="$(lua_get_lmod_dir)"

	popd > /dev/null || die
}

src_compile() {
	lua_foreach_impl lua_src_compile
}

lua_src_install() {
	pushd "${BUILD_DIR}" > /dev/null || die

	emake \
		DESTDIR="${ED}" \
		LUA_VERSION=skip \
		LUA_CMOD="$(lua_get_cmod_dir)" \
		LUA_LMOD="$(lua_get_lmod_dir)" \
		install

	popd > /dev/null || die
}

src_install() {
	lua_foreach_impl lua_src_install
	einstalldocs
}
