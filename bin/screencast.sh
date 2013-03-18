#!/usr/bin/env zsh

rec="/media/exchange/tmp/screencast-$(date +%Y%m%d-%H%M).rec.mp4"

function record() {
	ffmpeg -v info\
		-f x11grab -s 1920x1064 -i :0.0+0,16\
		-f pulse -i "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"\
		-f pulse -i "alsa_input.usb-Sennheiser_Communication_Sennheiser_USB_headset-00-headset.analog-mono"\
		-vcodec libx264 -preset ultrafast -tune zerolatency -crf 0\
		-acodec pcm_s16le\
		-filter_complex 'amerge, pan=2:c0=0.3*c0+3*c2:c1=0.3*c1+3*c2'\
		"$rec"
}

function recompress() {
	[[ ! -r "$1" ]] && echo "File not found or not readable: $1." && return 127

	## Mux to video
	fp="${1%%.*}.recomp.mp4"
	[[ -f $fp ]] && mv -f "$fp" "${fp}.bak"
	# This is gonna take a while
	ffmpeg -i $1 -vcodec libx264 -crf 19 -preset veryslow -tune film -acodec libfaac $fp

}

case $1 in
	recompress)
		recompress $2
		;;
	record)
		record $2
		;;
	*)
		echo "Usage: $0 [record|recompress]"
		;;
esac
