# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

PYTHON_COMPAT=( python3_{11..13} )

inherit flag-o-matic python-any-r1 toolchain-funcs

MY_PV="${PV//./}"
MY_P="mame-mame${MY_PV}"

DESCRIPTION="MAME developer tools (chdman, romcmp, jedutil, floptool, ...)"
HOMEPAGE="https://mamedev.org/ https://github.com/mamedev/mame"
SRC_URI="https://github.com/mamedev/mame/archive/refs/tags/mame${MY_PV}.tar.gz -> ${P}.tar.gz"
S="${WORKDIR}/${MY_P}"

LICENSE="GPL-2+ BSD BSD-2 Boost-1.0 CC0-1.0 LGPL-2.1 MIT ZLIB"
SLOT="0"
KEYWORDS="~amd64"

# MAME's tools-only build is not a supported upstream configuration: GENie
# eagerly evaluates osd_sdl's project definitions during Makefile generation,
# which drags in SDL2_ttf, fontconfig, freetype, OpenGL, X11, Qt5 (for the
# debugger module) and friends regardless of TOOLS=1 / EMULATOR=0. We follow
# the Arch/Debian pattern: build everything, install only the tool binaries.
RDEPEND="
	app-arch/zstd:=
	dev-libs/expat
	dev-libs/libutf8proc
	media-libs/flac
	media-libs/fontconfig
	media-libs/freetype
	media-libs/libglvnd
	media-libs/libsdl2
	media-libs/sdl2-ttf
	media-libs/portaudio
	media-libs/portmidi
	sys-libs/zlib
	x11-libs/libX11
	x11-libs/libXi
	x11-libs/libXinerama
"
DEPEND="${RDEPEND}
	dev-qt/qtwidgets:5
"
BDEPEND="
	${PYTHON_DEPS}
	virtual/pkgconfig
"

DOCS=( README.md )

src_compile() {
	tc-export CC CXX AR

	# Portage sets ARCH=amd64 in the build env (Gentoo profile var). MAME's
	# bundled GENie sub-makefile (3rdparty/genie/build/gmake.linux/genie.make)
	# pastes $(ARCH) onto the gcc command line as a flag, which makes gcc
	# treat "amd64" as an input file and fail. MAME itself doesn't use ARCH
	# (it uses ARCHITECTURE/ARCHOPTS); clear it for the sub-make.
	emake \
		ARCH= \
		PYTHON_EXECUTABLE="${EPYTHON}" \
		TOOLS=1 \
		USE_QTDEBUG=0 \
		OSD=sdl \
		TARGETOS=linux \
		NOWERROR=1 \
		VERBOSE=1 \
		OPTIMIZE=2 \
		PTR64=1 \
		SEPARATE_BIN=1 \
		USE_SYSTEM_LIB_EXPAT=1 \
		USE_SYSTEM_LIB_ZLIB=1 \
		USE_SYSTEM_LIB_ZSTD=1 \
		USE_SYSTEM_LIB_FLAC=1 \
		USE_SYSTEM_LIB_UTF8PROC=1
}

src_install() {
	local tools=(
		castool chdman floptool imgtool jedutil
		ldresample ldverify nltool nlwav pngcmp
		regrep romcmp srcclean testkeys unidasm
	)

	local bindir=build/linux_gcc/bin/x64/Release
	[[ -d ${bindir} ]] || die "could not locate built binary directory at ${bindir}"

	local t
	for t in "${tools[@]}"; do
		[[ -x ${bindir}/${t} ]] || die "missing built binary: ${t}"
		dobin "${bindir}/${t}"
	done

	# rename to avoid collision with sys-apps/coreutils' split(1)
	newbin "${bindir}/split" mame-split

	# install only the tool man pages we ship binaries for; skip ldplayer.1
	# and mame.6 which belong to the emulator
	local man
	for man in castool chdman floptool imgtool jedutil ldresample ldverify romcmp; do
		doman "docs/man/${man}.1"
	done

	einstalldocs
}
