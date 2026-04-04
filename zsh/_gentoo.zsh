# System services (OpenRC)
alias ss='sudo rc-service'
alias ssl='sudo rc-status'
alias sse='sudo rc-update add'        # enable service
alias ssd='sudo rc-update del'        # disable service

# User services (OpenRC 0.62+)
alias ssu='rc-service --user'
alias ssul='rc-update --user show'

# No direct equivalent to svlogtail — OpenRC logs via syslog
# If you install app-admin/sysklogd or similar:
alias jj='sudo tail -f /var/log/messages'
alias jjs='sudo tail -f /var/log/syslog'

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
alias xq='equery list'                # query installed
alias xqi='equery list -op'           # query all including overlays


# What got updated recently
alias updated="qlop -lH"
