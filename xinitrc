#!/bin/sh
#
# mkaito's X.org init file on Arch GNU/Linux

# {{{ Environment settings
export OOO_FORCE_DESKTOP="gnome"
errorlog="${HOME}/.xsession-errors"
gnupglog="${HOME}/.gnupg/gpg-agent.info"
# }}}

# {{{ Keyboard settings
setxkbmap es
xmodmap ~/.xmodmap
#xbindkeys &
# }}}

# {{{ Autostart
# Volume magement
thunar --daemon &

# Networkmanager
wicd-client &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &

# Open a terminal emulator on the first tag
#   - resume the old screen session or start a new one
urxvt -e screen -l -UDRS term &

# Open firefox
# firefox &
#}}}

# {{{ Start session

# Gotta love lightweight
# exec ck-launch-session openbox-session
exec ck-launch-session awesome
# }}}
