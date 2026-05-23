# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit desktop xdg

MY_PN="RimSort"
MY_PV="v${PV}"
MY_P="${MY_PN}-${MY_PV}"

DESCRIPTION="Open-source mod manager for RimWorld"
HOMEPAGE="https://github.com/RimSort/RimSort"
SRC_URI="
	amd64? ( https://github.com/RimSort/RimSort/releases/download/${MY_PV}/${MY_P}-Ubuntu-24.04_x86_64.tar.gz )
	https://raw.githubusercontent.com/RimSort/RimSort/${MY_PV}/data/io.github.rimsort.RimSort.desktop
	https://raw.githubusercontent.com/RimSort/RimSort/${MY_PV}/data/io.github.rimsort.RimSort.metainfo.xml
"
S="${WORKDIR}/${MY_PN}"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="-* ~amd64"
RESTRICT="bindist mirror strip test"

# Nuitka standalone bundle ships its own Python, Qt, and all wheels.
# Only system libc/libm/ld are dlopened by the entry binary.
RDEPEND=""

QA_PREBUILT="opt/${MY_PN}/*"

src_unpack() {
	unpack "${MY_P}-Ubuntu-24.04_x86_64.tar.gz"
}

src_install() {
	dodir /opt
	cp -a "${S}" "${ED}/opt/" || die "Failed to install bundle"

	dosym -r "/opt/${MY_PN}/${MY_PN}" "/usr/bin/${MY_PN}"

	domenu "${DISTDIR}/io.github.rimsort.RimSort.desktop"

	insinto /usr/share/icons/hicolor/scalable/apps
	newins "${S}/themes/default-icons/RimSort_Icon_64x64.svg" \
		io.github.rimsort.RimSort.svg

	insinto /usr/share/metainfo
	doins "${DISTDIR}/io.github.rimsort.RimSort.metainfo.xml"
}
