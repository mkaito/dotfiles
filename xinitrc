#!/bin/sh
#
# mkaito's X.org init file on Arch GNU/Linux

# {{{ Environment settings
export OOO_FORCE_DESKTOP="gnome"
errorlog="${HOME}/.xsession-errors"
gnupglog="${HOME}/.gnupg/gpg-agent.info"
export DE=xfce
export WINEPREFIX=${HOME}/.wine_64
# }}}

# {{{ Get my shell aliases
source ~/dev/dotfiles/alias.zshrc
# }}}

# {{{ Keyboard settings
setxkbmap es
xmodmap ~/.xmodmap
#xbindkeys &
# }}}

# {{{ Autostart
# Volume magement
# thunar --daemon &

# gnome-settings-daemon &

# Music Player Daemon
mpd ~/.mpd/mpd.conf

# Power stuff
#gnome-power-manager &
# xscreensaver -no-splash &

# Dropbox. Used to sync MobileOrg
dropboxd &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &

# Open a terminal emulator on the first tag
#   - resume the old screen session or start a new one
#urxvt -e screen -l -UDRS term &
urxvtd &

# Start the GnuPG agent and enable OpenSSH agent emulation
if (pgrep -u "${USER}" gpg-agent); then
  eval `cat ${gnupglog}`
  eval `cut -d= -f1 ${gnupglog} | xargs echo export`
else
  eval `gpg-agent --enable-ssh-support --daemon`
fi

# DBUS Session for nautilus, etc. 
if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

start-pulseaudio-x11 &
#}}}

# {{{ Start session
exec ck-launch-session awesome
# }}}
