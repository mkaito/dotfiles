#!/bin/sh
# Generate mise-${PV}-vendor.tar.xz next to this script.
#
# The ebuild expects the resulting tarball at:
#   ${FILESDIR}/mise-${PV}-vendor.tar.xz
#
# Usage:
#   ./make-vendor.sh 2026.5.16

set -eu

PV="${1:?usage: make-vendor.sh PV}"
P="mise-${PV}"
here="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
out="${here}/${P}-vendor.tar.xz"

if [ -f "${out}" ]; then
	echo "${out} already exists - delete it first to regenerate" >&2
	exit 1
fi

command -v cargo  >/dev/null || { echo "cargo missing"  >&2 ; exit 1 ; }
command -v git    >/dev/null || { echo "git missing"    >&2 ; exit 1 ; }
command -v tar    >/dev/null || { echo "tar missing"    >&2 ; exit 1 ; }
command -v xz     >/dev/null || { echo "xz missing"     >&2 ; exit 1 ; }

work="$(mktemp -d)"
trap 'rm -rf "${work}"' EXIT

cd "${work}"
git clone --depth 1 --branch "v${PV}" \
	https://github.com/jdx/mise.git "${P}"

cd "${P}"
cargo vendor --locked --versioned-dirs vendor > /dev/null

tar -caf "${out}" vendor

printf 'wrote %s (%s bytes)\n' "${out}" "$(stat -c%s "${out}")"
