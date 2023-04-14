#!/usr/bin/env bash
# wrapper for Tarsnap

# Copyright (c) 2013-2018 Jason W Ryan
# The MIT License (MIT) http://opensource.org/licenses/MIT


box="hydra"
dir=('/home/chris' '/etc')

# base commands
arc="-c -f $box-$(date +%d%m%y_%H:%M) ${dir[@]}"                  # archive
tst="-c -f $box-$(date +%d%m%y_%H:%M) --dry-run -v ${dir[@]}"     # dry run
del="-d -f $box-"                                          # delete archive
ext="-x -f $box-"                                          # extract archive

usage () {
	cat <<EOF
	Tarsnap options:
	  -a: archive  |   Create and upload a new snapshot
	  -d: delete   |   Delete a snapshot
	  -e: extract  |   Extract an archive
	  -l: list     |   List all current snapshots
	  -p: print    |   Print files in selected archive
	  -s: summary  |   Print a summary
	  -t: test     |   Dry-run a new snapshot
	  -h: help     |   Show these options
EOF
}

check () {
	while true; do
		read -p "Which archive? " archive
		# check for an archive number
		if [[ $archive =~ ^[0-9]{6}_[0-9]{2}:[0-9]{2}$ ]]; then
			case "$opt" in
				-d) op="${del}${archive}"
					break
					;;
				-e) op="${ext}${archive}"
					break
					;;
				-p) op="-tv -f ${box}-${archive}"
					printf "%s\n" "Printing files to tarsnap.files"
					break
					;;
				 *) printf "%s\n" "Not a valid choice."
					break
					;;
			esac
		else
			printf "%s\n\n" "Numbers, please..."
		fi
	done
}

dryrun () {
	printf "%s\n" "Starting dry run: results saved to tarsnapout.txt"
	op="${tst}"
}

opt="$1"
[[ -z $opt ]] && usage && exit 1

case "$1" in
	-a) op="${arc}"
		;;
	-l) op="--list-archives"
		;;
	-s) op="--print-stats"
		;;
  -k) op="$op --keyfile $2"
    ;;
  -d|-e|-p) check
		;;
	-t) dryrun
		;;
	-h|*) usage && exit 1
		;;
esac

# run the options
case "$1" in
	-t) sudo tarsnap $op 2>tarsnapout.txt
		;;
	-p) sudo tarsnap $op >tarsnap-${archive}.files
		;;
	 *) sudo tarsnap $op
		;;
esac
