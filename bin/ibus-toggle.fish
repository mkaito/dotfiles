#!/usr/bin/fish
set -l current (ibus engine)
set -l spanish "xkb:es::spa"
set -l english "xkb:us::eng"

if test $current = $english
	ibus engine $spanish
	notify-send "Layout: Spanish"
else
	ibus engine $english
	notify-send "Layout: English"
end
