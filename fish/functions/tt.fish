function tt --wraps=transmission-remote
	set -l torrents
	set -l tline

	if not isatty stdin
		while read -l line
			# `grep` will only print the matching portion of lines that start with whitespace followed by digits.
			# `tr` then removes any whitespace.
			set torrents $torrents (echo $line | grep -Po '^\s*\d+' | tr -d ' ')
		end

		echo "Got torrents: $torrents."

		# Makeshift hacky version of $torrents.join(',')
		set tline $torrents[1]
		if test (count $torrents) -ge 2
			for t in $torrents[2..-1]
				set tline $tline","$t
			end
		end
	end

	if test -n $tline
		command transmission-remote -t$tline $argv
	else
		command transmission-remote $argv
	end
end
