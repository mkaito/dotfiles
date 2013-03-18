#!/bin/sh
#
# mkaito's X.org init file on Arch GNU/Linux

# {{{Environment settings
## Fix WM compat for some apps
export OOO_FORCE_DESKTOP="gnome"
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
export JAVA_FONTS=/usr/share/fonts/TTF
# wmname LG3D
export DE=xfce

# Keep the logs
export errorlog="${HOME}/.xsession-errors"
export gnupglog="${HOME}/.gnupg/gpg-agent.info"

# I should probably just move this to .wine
export WINEPREFIX=${HOME}/.wine_64

# Ibus
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus
# export GTK_IM_MODULE_FILE=/etc/gtk-2.0/gtk.immodules

# MPD runs on a unix socket
export MPD_HOST=${HOME}/.mpd/mpd.sock
export MPD_PORT=-1
#}}}

# {{{ Resource and keymap settings
# usermodmap="${HOME}/.Xmodmap"
# userresources="${HOME}/.Xresources"
# userdefresources="${HOME}/.Xresources"
# sysmodmap="/etc/X11/xinit/.Xmodmap"
# sysresources="/etc/X11/xinit/.Xresources"

# # Merge system and user resources and keymaps
# [[ -r "${sysresources}" ]]  && xrdb -merge "${sysresources}"
# [[ -r "${sysmodmap}" ]]     && xmodmap "${sysmodmap}"
# [[ -r "${userresources}" ]] && xrdb -merge "${userresources}"
# [[ -r "${userdefresources}" ]] && xrdb -merge "${userdefresources}"
# #[[ -r "${usermodmap}" ]]    && xmodmap "${usermodmap}"
xrdb ~/.Xresources &
# }}}

# {{{ Keyboard settings
#setxkbmap es
#xmodmap ~/.xmodmap
ibus-daemon -r -d &
#autocutsel -fork &
#autocutsel -selection PRIMARY -fork &
#}}}

# {{{ Expand the font search path
[[ -d /usr/share/fonts/local ]] && xset +fp /usr/share/fonts/local
[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts
xset fp rehash &
#}}}

# {{{ Autostart programs
hostname=$(uname -n)

function checkstart() {
	pidof $1 > /dev/null || $@
}

[[ "$hostname" == "archbox" ]] && sudo VBoxClient-all &

# Start urxvt daemon
#urxvtd -q -o -f &

# SSH Agent
eval `ssh-agent`

checkstart bitcoind -daemon &

# Instead of starting it here every single time,
# I've decided to have scripts call it themselves.
# That way, if a script needs unattended ssh, it can ask for it,
# and otherwise, I won't be bothered by the askpass dialog.
# (ssh-add -l || ssh-add) &

# Music Player Daemon
checkstart mpd ~/.mpd/mpd.conf &
checkstart mpdscribble &

# SSH Tunnels
## SSH listening on 2304
#autossh -M 2305 -fNR 2304:localhost:2304 mkaito

# Dropbox. Used to sync MobileOrg
#dropboxd &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon 1>/dev/null &

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"

if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
  eval `cat $gnupginf`
  eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    #eval `gpg-agent --enable-ssh-support --daemon`
    eval `gpg-agent --daemon`
fi

xsetroot -cursor_name left_ptr &

# Kill the goddamn bell
xset b 0 &

# Don't turn the screen off
xset -dpms
xset s off

# Turn numlock on
# numlockx &

# Remove the mouse pointer when it's idle
# This screws with window focus
#checkstart unclutter -idle 3 &

# Redshift adjusts monitor colour temperature according to day time
checkstart redshift -l 28:-13 -t 5600:5000 -m vidmode &

# Fix alsa volumes
if [[ "$hostname" == "hydra" ]]; then
	(sleep 10 && amixer -c 0 set Center 100+ unmute) &
	(sleep 10 && amixer -c 0 set LFE 100+ unmute) &
	(sleep 10 && amixer -c 0 set Side 100+ unmute) &
else
	(sleep 10 && amixer set Master 100+ unmute) &
fi

# Keep the sc2replay folder clean
if [[ "$hostname" == "hydra" ]]; then
	pgrep 'upload_sc2replays.sh' || $HOME/dev/bin/upload_sc2replays.sh &
fi;

# Canto RSS
checkstart canto-fetch -b &
#}}}

# {{{ WM
case $1 in
	subtle)
		exec dbus-launch subtle --no-randr
		;;
	urxvt)
		urxvt
		;;
	*) # dwm
		#conky -c ~/dev/dotfiles/dwm/conky_status | while read -r; do xsetroot -name "$REPLY"; done &
		nitrogen --restore &
		exec dbus-launch ~/dev/bin/startdwm
		;;
esac
                    
#}}}

# vim:fdm=marker
