#!/usr/bin/env fish
# `t` is a wrapper/helper script for the tintin++ MUD client
set -l ttroot ~/dev/tintin

if test \( -n $argv \) -a \( -d $ttroot/$argv[1] \)
	if test -f $ttroot/$argv[1]/init.tt
		cd $ttroot/$argv[1]
		exec tintin -G init.tt
	else
		echo 'init.tt not found in $argv[1]'
	end
else
	echo -n 'Please provide a valid mud handle: '
	for mud in $ttroot/*
		echo -n (basename $mud)" "
	end
	echo '.'
end
# vim:ft=fish:noet
