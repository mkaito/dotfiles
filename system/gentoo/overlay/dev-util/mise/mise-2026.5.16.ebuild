# Copyright 2026 Christian Höppner
# Distributed under the terms of the GNU General Public License v2

# Forked from guru/dev-util/mise after upstream last-rited on 2026-05-18
# (https://bugs.gentoo.org/974977). The CRATES= list of ~600 crates is
# impractical to maintain by hand; the vendor tarball is produced by
# files/make-vendor.sh and stored in FILESDIR. Bump it whenever PV bumps:
#   mise run gentoo:vendor -- --force

EAPI=8

RUST_MIN_VER="1.91.0"

inherit cargo

DESCRIPTION="Dev tools, env vars, and tasks in one CLI"
HOMEPAGE="https://mise.jdx.dev https://github.com/jdx/mise"
SRC_URI="https://github.com/jdx/${PN}/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"

# mise is MIT; the vendored crates contribute a wide mix of permissive
# licences. Listing the union of every crate licence would be cargo-license
# output noise - patch this when ultraviolet over a release.
LICENSE="MIT Apache-2.0 BSD BSD-2 CDLA-Permissive-2.0 ISC MPL-2.0 openssl Unicode-3.0 ZLIB BZIP2 || ( CC0-1.0 MIT-0 )"
SLOT="0"
KEYWORDS="-* ~amd64"
RESTRICT="mirror"

# openssl-sys links system openssl at build + runtime. Everything else in
# the dependency graph is either pure-rust or vendored via *-rs crates
# (zlib-rs, libbz2-rs-sys, lzma-rust2, ...) so no other system libs are
# dlopened by the built binary (verified via ldd on the guru build).
DEPEND="dev-libs/openssl:="
RDEPEND="${DEPEND}"
BDEPEND="virtual/pkgconfig"

src_unpack() {
	default

	if [[ ! -f ${FILESDIR}/${P}-vendor.tar.xz ]]; then
		eerror "Missing vendored crates tarball:"
		eerror "    ${FILESDIR}/${P}-vendor.tar.xz"
		eerror
		eerror "Generate it with:"
		eerror "    mise run gentoo:vendor -- --force"
		eerror "or directly:"
		eerror "    ${FILESDIR}/make-vendor.sh ${PV}"
		die "vendor tarball missing"
	fi

	mkdir -p "${ECARGO_VENDOR}" || die
	tar -xf "${FILESDIR}/${P}-vendor.tar.xz" \
		--strip-components=1 -C "${ECARGO_VENDOR}" \
		|| die "vendor unpack failed"

	cargo_gen_config
}

src_install() {
	cargo_src_install

	doman man/man1/mise.1

	# Suppress upstream's self-update; point at the overlay workflow instead.
	insinto /usr/lib/mise
	doins "${FILESDIR}"/mise-self-update-instructions.toml

	# Paranoid mode system-wide.
	insinto /etc/mise
	doins "${FILESDIR}"/config.toml

	# Shell auto-activation hooks (bash, zsh, fish).
	insinto /etc/profile.d
	doins "${FILESDIR}"/mise.sh

	insinto /usr/share/fish/vendor_conf.d
	doins "${FILESDIR}"/mise.fish
}
