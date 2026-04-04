# Package management
alias xi='sudo emerge -a'
alias xis='eix'
xr() {
    sudo emerge --ask --unmerge "$@"
    sudo emerge --ask --depclean
}

# Upgrade
alias xiu='sudo emaint sync -a && sudo emerge -auDN @world'
alias xiuu='sudo emaint sync -a && sudo emerge -auDNv @world'

# Query
alias xq='equery list'
alias xqi='equery list -op'

# What got updated recently
alias updated="qlop -lH"

# vim:ft=zsh
