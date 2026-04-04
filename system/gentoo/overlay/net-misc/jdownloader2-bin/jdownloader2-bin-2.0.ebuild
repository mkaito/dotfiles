# Copyright 2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit desktop

DESCRIPTION="JDownloader 2 download manager (binary launcher)"
HOMEPAGE="https://jdownloader.org/"
SRC_URI="http://installer.jdownloader.org/JDownloader.jar -> JDownloader-${PV}.jar"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64"

RDEPEND="virtual/jre:*"

S="${WORKDIR}"

RESTRICT="bindist mirror strip"


src_unpack() {
	:
}

src_install() {
	local dest="/opt/jdownloader2"

	insinto "${dest}"
	newins "${DISTDIR}/JDownloader-${PV}.jar" JDownloader.jar

	cat > "${T}/jdownloader2" <<-'EOF'
	#!/bin/sh
	JD_HOME="${HOME}/.jdownloader2"
	JD_JAR="${JD_HOME}/JDownloader.jar"

	if [ ! -d "${JD_HOME}" ]; then
		mkdir -p "${JD_HOME}"
	fi

	if [ ! -f "${JD_JAR}" ]; then
		cp /opt/jdownloader2/JDownloader.jar "${JD_JAR}"
	fi

	cd "${JD_HOME}" || exit 1
	exec java -jar "${JD_JAR}" "$@"
	EOF
	dobin "${T}/jdownloader2"

	cat > "${T}/jdownloader2.desktop" <<-EOF
	[Desktop Entry]
	Version=1.0
	Type=Application
	Name=JDownloader 2
	Comment=Java-based download manager
	Exec=jdownloader2
	Categories=Network;FileTransfer;
	StartupNotify=true
	Terminal=false
	EOF
	domenu "${T}/jdownloader2.desktop"
}

pkg_postinst() {
	elog "On first run, JDownloader will bootstrap itself into ~/.jdownloader2/"
	elog "and manage its own updates from there."
}
