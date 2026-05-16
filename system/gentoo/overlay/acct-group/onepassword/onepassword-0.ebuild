# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8
inherit acct-group

# 1Password desktop's verify_ipc_peer rejects peer gid < 1000 (and != nogroup).
# See verify_ipc_peer in /opt/1Password/resources/app.asar.unpacked/index.node
# around offset 0x52fc509: gid >> 3 must be >= 125. Affects op CLI, browser
# integration and biometric unlock. Force gid >= 1000.
ACCT_GROUP_ID=1100
