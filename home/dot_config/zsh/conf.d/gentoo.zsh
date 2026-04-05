# Package management
alias xi='sudo emerge -a'
alias xis='eix'
xr() {
    sudo emerge --ask --unmerge "$@"
    sudo emerge --ask --depclean
}

# What got updated recently
alias updated="qlop -lH"

# vim:ft=zsh
