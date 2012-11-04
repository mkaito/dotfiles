#!/bin/bash
# Use inotify to watch for new replays and upload

# Make sure we can authenticate.
ssh-add -l || ssh-add

# Wherever Sc2gears drops your auto-saved replays
folder='/home/chris/Starcraft II/Accounts/123674966/2-S2-1-2307716/Replays/Multiplayer'

# Clean, upload, and then wait for new replays. Rinse and repeat.
while true
do
	# We will delete any replays from the Unit Preloader map right when they are created.
	# Upload any replay that isn't against a computer, but do not delete vs AI replays,
	# since they are used in build refining.

	# The patterns below assume that {/p2} and {/m} are present in the replay file name.
	# Configure replay renaming in Sc2gears.

	#find "$folder" -name '*Player 2*' -o -name '*Computer 2*' -o -name '*Unit Preloader*' -delete
	find "$folder" -name '*Unit Preloader*' -delete
	rsync -rz --delete-after "$folder"/ \
		--exclude='*Player 2*' --exclude='*Computer 2*'\
		mkaito:/srv/http/files.mkaito.com/public/sc2replay/
	inotifywait -rqe create "$folder"
done
