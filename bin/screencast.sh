#!/usr/bin/env zsh

of="$(mktemp -u --tmpdir=${HOME}/.tmp).of.mkv"

rec="$(mktemp -u --tmpdir=${HOME}/.tmp).rec.mkv"
#recspeakers="$(mktemp -u --tmpdir=${HOME}/.tmp).recspeakers.wav"
#recmicrophone="$(mktemp -u --tmpdir=${HOME}/.tmp).recmicrophone.wav"
vo="$(mktemp -u --tmpdir=${HOME}/.tmp).vo.mp4"
ao="$(mktemp -u --tmpdir=${HOME}/.tmp).ao.mp3"

vorecomp="$(mktemp -u --tmpdir=${HOME}/.tmp).vorecomp.mp4"

noiseprofile="$(mktemp -u --tmpdir=${HOME}/.tmp).noise-profile"
afiltered="$(mktemp -u --tmpdir=${HOME}/.tmp).afiltered.mp3"

#micfiltered="$(mktemp -u --tmpdir=${HOME}/.tmp).micfiltered.wav"
#speakersfiltered="$(mktemp -u --tmpdir=${HOME}/.tmp).speakersfiltered.wav"
#aorecomp="$(mktemp -u --tmpdir=${HOME}/.tmp).aorecomp.ogg"

trap cleanup SIGHUP SIGINT SIGQUIT SIGKILL SIGTERM

function cleanup() {
	echo "Cleaning up..."
	rm -rf "$rec" "$vo" "$ao" "$vorecomp" "$noiseprofile" "$afiltered"
}

function record() {
	echo "Starting capture"
	#pacat -d 0 --record | sox -t raw -r 44100 -s -L -b 16 -c 2 - "$recspeakers" &
	#pacat -d 3 --record | sox -t raw -r 44100 -s -L -b 16 -c 2 - "$recmicrophone" &

	ffmpeg -v info -f x11grab -s 1920x1080 -r 30000/1001 -i :0.0\
		-f alsa -i pulse\
		-vcodec libx264 -crf 0 -s 1280x720\
		-acodec libmp3lame\
		"$rec"

	# Kill pacat as source, else sox will get a broken pipe error
	pkill pacat
}

function recompress() {
	## Merge and filter audio
	#sox -c 2 "$recmicrophone" -n trim 0 1 noiseprof "$noiseprofile"
	#sox -c 2 "$recmicrophone" "$micfiltered" noisered "$noiseprofile" 0.05
	#sox -v 0.6 "$recspeakers" "$speakersfiltered"
	#normalize "$micfiltered"
	#sox --combine merge "$micfiltered" "$speakersfiltered" "$aorecomp"
	#normalize-ogg "$aorecomp"

	mkvextract tracks "$rec" "0:$vo" "1:$ao" 

	## Mux to video
	mencoder -oac copy -ovc x264 -x264encopts crf=18:preset=veryslow -mc 0 -noskip "$vo" -o "$vorecomp"
	#x264 "$recvideo" --pass 1 --bitrate 1000 --preset placebo -o /dev/null
	#x264 "$recvideo" --pass 2 --bitrate 1000 --preset placebo -o "$vorecomp"
	#x264 "$recvideo" --crf 18 --preset slow -o "$vorecomp"

	sox "$ao" -n trim 0 1 noiseprof "$noiseprofile"
	sox "$ao" "$afiltered" noisered "$noiseprofile" 0.05
	normalize-mp3 "$afiltered"

	mkvmerge -o "$of" "$afiltered" "$vorecomp"

	echo "*** Please type an extra string to be inserted in the file name."
	echo "*** Format: \"test-game-against-very-hard-ai\""
	echo -n "String: "
	read n
	[[ ! -z "$n" ]] && n="${n}-"
	fp="${HOME}/dev/mydropbox/sync/public/fpvod/mkaito-fpvod-${n}$(date +%Y%m%d-%H%M).mkv"
	mv "$of" "$fp"
}

record; recompress; cleanup
