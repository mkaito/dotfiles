# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit udev

DESCRIPTION="udev rules for 8BitDo USB Wireless Adapter (U2W)"
HOMEPAGE="https://www.8bitdo.com/"
SRC_URI=""
S="${WORKDIR}"

LICENSE="public-domain"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="virtual/udev"

src_install() {
	udev_dorules "${FILESDIR}/71-8bitdo-u2w.rules"
}

pkg_postinst() {
	udev_reload
}

pkg_postrm() {
	udev_reload
}
