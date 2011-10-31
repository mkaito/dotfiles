#!/bin/sh
#
# mkaito's X.org init file on Arch GNU/Linux

# {{{ Environment settings
export OOO_FORCE_DESKTOP="gnome"
export errorlog="${HOME}/.xsession-errors"
export gnupglog="${HOME}/.gnupg/gpg-agent.info"
export DE=xfce
export WINEPREFIX=${HOME}/.wine_64
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export GTK_IM_MODULE_FILE=/etc/gtk-2.0/gtk.immodules
export MPD_HOST=${HOME}/.mpd/mpd.sock
export MPD_PORT=-1
# }}}

# {{{ Keyboard settings
setxkbmap es
xmodmap ~/.xmodmap
ibus-daemon -r -d &
#autocutsel -fork &
#autocutsel -selection PRIMARY -fork &
# }}}

# {{{ Expand the font search path
xset +fp /usr/share/fonts/local &
xset fp rehash &
# }}}

# {{{ Autostart
# Music Player Daemon
mpd ~/.mpd/mpd.conf &
scmpc &

# Dropbox. Used to sync MobileOrg
dropboxd &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &

urxvtd -q -f -o &

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"

if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
  eval `cat $gnupginf`
  eval `cut -d= -f1 $gnupginf | xargs echo export`
else
  eval `gpg-agent --enable-ssh-support --daemon`
fi

# DBUS Session for nautilus, etc. 
#if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    #eval `dbus-launch --sh-syntax --exit-with-session`
#fi

xsetroot -cursor_name left_ptr &
# Mouse speed (intended for my Razer @ 4000 dpi)
xset m 1/2 1 &
xinput --set-prop "pointer:Razer Razer Lachesis" "Device Accel Constant Deceleration" 2 &
# I'll have some faster scrolling with that too.
xinput --set-prop "pointer:Razer Razer Lachesis" "Evdev Wheel Emulation Inertia" 15 &
#}}}

# {{{ Start session
#exec ck-launch-session awesome > .awesome.log
#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
#     --expand true --width 10 --transparent false --tint 0x191970 --height 14 &
nitrogen --restore &
#exec ck-launch-session xmonad
conky -c ~/dev/dotfiles/dwm/conky_status | while read -r; do xsetroot -name "$REPLY"; done &
exec ~/dev/bin/startdwm
# }}}
