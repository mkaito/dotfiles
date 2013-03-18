#!/bin/zsh
# Use inotify to watch for new replays and upload

# Make sure we can authenticate.
ssh-add -l || ssh-add

# Wherever Sc2gears drops your auto-saved replays
folder='/home/chris/Starcraft II/Accounts/123674966/2-S2-1-2307716/Replays/Multiplayer'

# Reactive cleaning: I might have been playing on windows.
find "$folder" -name '*Unit Preloader*' -delete

# Clean, upload, and then wait for new replays. Rinse and repeat.
while true
do
	f=`inotifywait -rqe create  --format %w "$folder"`
	echo "Received event for file $f"

	if [[ "$f" =~ "^.*Unit Preloader.*$" ]]; then
		echo "Found Unit Preloader map. Exterminate! Exterminate!"
		rm "$f"
	fi

	if [[ ! "$f" =~ "^.*(Player 2|Computer 2).*$" ]]; then
		echo "Found a ladder map. Purging chat data."
		find "$f" -execdir smpq -a -f {} replay.message.events \;
	fi

	rsync -rz --delete-after "$folder"/ \
		--exclude='*Player 2*' --exclude='*Computer 2*' --exclude 'replay.message.events'\
		mkaito:/srv/http/files.mkaito.com/public/sc2replay/
done
