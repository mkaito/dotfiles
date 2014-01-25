#!/bin/zsh
#
# mkaito's X.org init file on Arch GNU/Linux

## Fix WM compat for some apps
export OOO_FORCE_DESKTOP="gnome"
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
#export BSPWM_SOCKET=~/.bspwm_sock
export JAVA_FONTS=/usr/share/fonts/TTF
wmname LG3D
export DE=xfce
export LANG="en_US.UTF-8"

# Keep the logs
export errorlog="${HOME}/.xsession-errors"
export gnupglog="${HOME}/.gnupg/gpg-agent.info"

# I should probably just move this to .wine
#export WINEPREFIX=${HOME}/.wine_64

# Ibus
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=ibus
#export GTK_IM_MODULE_FILE=/etc/gtk-2.0/gtk.immodules

# MPD runs on a unix socket
export MPD_HOST=${HOME}/.mpd/mpd.sock
export MPD_PORT=-1

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

#setxkbmap es
#xmodmap ~/.xmodmap
ibus-daemon -r -d &
#autocutsel -fork &
#autocutsel -selection PRIMARY -fork &

[[ -d /usr/share/fonts/local ]] && xset +fp /usr/share/fonts/local
[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts
xset fp rehash &

hostname=$(uname -n)

function checkstart() {
	pgrep -c $1 || $@
}

[[ $hostname == "archbox" ]] && sudo VBoxClient-all &

# Source scripts from /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Start urxvt daemon
urxvtd -q -o -f

# SSH Agent
#eval `ssh-agent`
#(ssh-add -l || ssh-add) &

(sleep 300 && checkstart bitcoind -daemon) &

# Music Player Daemon
checkstart mpd ~/.mpd/mpd.conf &
checkstart mpdscribble &

# SSH Tunnels
## SSH listening on 2304
autossh -M 2305 -fNR 2304:localhost:2304 mkaito

# Dropbox. Used to sync MobileOrg
dropboxd &

# Start the GNU Emacs daemon
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon 1>/dev/null &

# Start the GnuPG agent and enable OpenSSH agent emulation
gnupginf="${HOME}/.gnupg/gpg-agent.info"

if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
  eval `cat $gnupginf`
  eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --enable-ssh-support --daemon`
    #eval `gpg-agent --daemon`
fi

xsetroot -cursor_name left_ptr

# Kill the goddamn bell
xset b off

# Don't turn the screen off
xset -dpms
xset s off

# Turn numlock on
#numlockx &

# Remove the mouse pointer when it's idle
# This screws with window focus
#checkstart unclutter -idle 3 -noevents &

# Redshift adjusts monitor colour temperature according to day time
checkstart redshift -l 28:-13 -t 5600:4000 -m randr &!

# Fix alsa volumes
if [[ "$hostname" == "hydra" ]]; then
	(sleep 10 && amixer -c 0 set Center 100+ unmute) &!
	(sleep 10 && amixer -c 0 set LFE 100+ unmute) &!
	(sleep 10 && amixer -c 0 set Side 100+ unmute) &!
else
	(sleep 10 && amixer set Master 100+ unmute) &!
fi

# Keep the sc2replay folder clean
#if [[ "$hostname" == "hydra" ]]; then
	#checkstart upload_sc2replays.sh &!
#fi;

checkstart generate_ledger_reports &!

# Canto RSS
#checkstart canto-fetch -b -s -i 300 &
checkstart canto-daemon &

# Torrents
#(sleep 60 && checkstart deluged) &

# Get the bitcoin miner running in the background
#pgrep -c cgminer || screen -S cgminer -d -m cgminer &

nitrogen --restore &

# Slim doesn't set the default session, even if it says it does in a comment in the conf file.
# Using parameter expansion below to set a "default" session instead.
case "${1:-dwm}" in
	subtle)
		exec subtle --no-randr > ~/subtle.log 2>&1
		;;
	bspwm)
		sxhkd &
		exec wm
		;;
	dwm) # dwm
		#conky -c ~/dev/dwm/conkyrc | while read -r; do xsetroot -name "$REPLY"; done &
		exec ~/dev/bin/startdwm
		;;
	wmfs)
		exec wmfs
		;;
	*) # Pop a shell to fix things
		exec urxvt
esac
