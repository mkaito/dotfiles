# This shell script is run before Openbox launches.
# Environment variables set here are passed to the Openbox session.

# Set background
fbsetbg -l

# D-bus
if which dbus-launch >/dev/null 2>&1 && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
       eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Run XDG autostart things.  By default don't run anything desktop-specific
# See xdg-autostart --help more info
DESKTOP_ENV="OPENBOX"
if which /usr/lib/openbox/xdg-autostart >/dev/null 2>&1; then
  /usr/lib/openbox/xdg-autostart $DESKTOP_ENV
fi

# (sleep 2 && pypanel) &
# (sleep 2 && netwmpager) &
# (sleep 2 && roxterm_desktop --geometry 127x5+0+0 -p Background) &
# (sleep 2 && terminator --name deskterm --geometry=1015x876+0+0 -l background -b) &
# (sleep 2 && sakura --name deskterm --geometry 127x25+0+0) &
# (sleep 2 && sakura --name deskterm --geometry 127x27+0+440) &
(sleep 2 && conky -c ~/.conky/_conkyrc_tower) &
(sleep 2 && conky -c ~/.conky/_conkyrc_clock) &
# (sleep 2 && lxpanel) &
# cairo-compmgr &
xcompmgr -c -C -n &
