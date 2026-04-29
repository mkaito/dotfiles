# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit udev

DESCRIPTION="udev rules for Flydigi Vader 4 Pro (DInput/hidraw access)"
HOMEPAGE="https://www.flydigi.com/"
SRC_URI=""
S="${WORKDIR}"

LICENSE="public-domain"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="virtual/udev"

src_install() {
	udev_dorules "${FILESDIR}/71-flydigi-vader4.rules"
}

pkg_postinst() {
	udev_reload
}

pkg_postrm() {
	udev_reload
}
