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

# Power manager
gnome-power-manager &

# Dropbox. Used to sync MobileOrg
dropboxd &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &

# Open a terminal emulator on the first tag
#   - resume the old screen session or start a new one
urxvt -e screen -l -UDRS term &

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"
 
if ( pgrep -u "${USER}" gpg-agent ); then
    eval `cat $gnupginf`
    eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --enable-ssh-support --daemon`
fi
#}}}

# {{{ Start session

# Gotta love lightweight
# exec ck-launch-session openbox-session
exec ssh-agent ck-launch-session awesome
# }}}
