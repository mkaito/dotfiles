#!/bin/sh
#
# mkaito's X.org init file on Arch GNU/Linux

# {{{ Environment settings
export OOO_FORCE_DESKTOP="gnome"
export errorlog="${HOME}/.xsession-errors"
export gnupglog="${HOME}/.gnupg/gpg-agent.info"
export DE=xfce
export WINEPREFIX=${HOME}/.wine_64
export XMODIFIERS=@im=SCIM
export GTK_IM_MODULE="scim"
export QT_IM_MODULE="scim"
# }}}

# {{{ Keyboard settings
setxkbmap es
xmodmap ~/.xmodmap
scim -d &
# }}}

# {{{ Autostart
# Music Player Daemon
mpd ~/.mpd/mpd.conf

# Dropbox. Used to sync MobileOrg
dropboxd &

# Start the GNU Emacs daemon
#[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &

urxvtd -q -f &
#urxvtc -e screen -l -UDRS term &

# Start the GnuPG agent and enable OpenSSH agent emulation
if (pgrep -u "${USER}" gpg-agent); then
  eval `cat ${gnupglog}`
  eval `cut -d= -f1 ${gnupglog} | xargs echo export`
else
  eval `gpg-agent --enable-ssh-support --daemon`
fi

# DBUS Session for nautilus, etc. 
#if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    #eval `dbus-launch --sh-syntax --exit-with-session`
#fi

#xsetroot -cursor_name left_ptr &
#}}}

# {{{ Start session
exec ck-launch-session awesome
#exec ck-launch-session gnome-session
# }}}
