#!/bin/zsh
#
# mkaito's X.org init file on Arch GNU/Linux

## Fix WM compat for some apps
export OOO_FORCE_DESKTOP="gnome"
export IBUS_ENABLE_SYNC_MODE=1
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dawt.useSystemAAFontSettings=lcd -Dsun.java2d.opengl=true'
export JAVA_FONTS=/usr/share/fonts/TTF
wmname LG3D
export DE=xfce
export LANG="en_US.UTF-8"

# Keep the logs
export errorlog="${HOME}/.xsession-errors"
export gnupglog="${HOME}/.gnupg/gpg-agent.info"

# Ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export GTK_IM_MODULE_FILE=/etc/gtk-2.0/gtk.immodules

# MPD runs on a unix socket
export MPD_HOST="${HOME}/.mpd/mpd.sock"
export MPD_PORT=-1

# DBUS bullshit
export DBUS_SESSION_BUS_ADDRESS=/run/user/$(id -u)/dbus/user_bus_socket

# Enable firefox hardware acceleration
export MOZ_USE_OMTC=1

xrdb ~/.Xresources

ibus-daemon -xrd

[[ -d /usr/share/fonts/local ]] && xset +fp /usr/share/fonts/local
[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts
xset fp rehash &

hostname=$(uname -n)

[[ $hostname == "archbox" ]] && sudo VBoxClient-all &

# Source scripts from /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"

if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
  eval `cat $gnupginf`
  eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --enable-ssh-support --daemon`
fi

# Fix cursor and set background
xsetroot -cursor_name left_ptr -solid "#101010"

# Kill the goddamn bell
xset b off

# Don't turn the screen off
xset -dpms
xset s off

# Fix alsa volumes
if [[ "$hostname" == "hydra" ]]; then
	(sleep 10 && amixer -c 0 set Center 100+ unmute) &!
	(sleep 10 && amixer -c 0 set LFE 100+ unmute) &!
	(sleep 10 && amixer -c 0 set Side 100+ unmute) &!
else
	(sleep 10 && amixer set Master 100+ unmute) &!
fi

# autocutsel -selection PRIMARY   -fork &!
# autocutsel -selection CLIPBOARD -fork &!

redshift -l 28:-13 -t 5000:3000 -b 0.85:0.85 -m randr &

# xrandr --output DVI-I-2 --primary &!

# Slim doesn't set the default session, even if it says it does in a comment in the conf file.
# Using parameter expansion below to set a "default" session instead.
case "${1:-i3}" in
    dwm)
        exec /home/chris/dev/bin/startdwm
        ;;
    i3)
        exec /home/chris/dev/bin/wm
        ;;
    *) # Pop a shell to fix things
        exec urxvt
esac
