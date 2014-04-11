#!/bin/zsh
#
# mkaito's X.org init file on Arch GNU/Linux

## Fix WM compat for some apps
export OOO_FORCE_DESKTOP="gnome"
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
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
export MPD_HOST=${HOME}/.mpd/mpd.sock
export MPD_PORT=-1

xrdb ~/.Xresources

ibus-daemon --xim -r -d &

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

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && DISPLAY='' emacs --daemon -nw --no-splash 1>/dev/null &

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"

if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
  eval `cat $gnupginf`
  eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --enable-ssh-support --daemon`
    #eval `gpg-agent --daemon`
fi

# Fix cursor
xsetroot -cursor_name left_ptr

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

nitrogen --restore &

# Slim doesn't set the default session, even if it says it does in a comment in the conf file.
# Using parameter expansion below to set a "default" session instead.
case "${1:-i3}" in
    i3)
        exec zsh -c "dbus-launch i3"
        ;;
    *) # Pop a shell to fix things
        exec urxvt
esac
