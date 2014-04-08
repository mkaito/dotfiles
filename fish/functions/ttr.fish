function ttr
	set -l torrents
  set -l tline

  if not isatty 0
    while read line
      set torrents $torrents (echo $line | grep -Po '^\s*\d+' | tr -d ' ')
    end

  else
    if test (count $argv) -ge 1
      set torrents $argv[1]

      if test (count $argv) -ge 2
        set argv $argv[2..-1]
      end
    else
      echo "Insufficient parameters."
      return 65
    end
  end

  ##
  # Makeshift hacky version of $torrents.join(',')
  set tline $torrents[1]
  if test (count $torrents) -ge 2
    for t in $torrents[2..-1]
      set tline $tline","$t
    end
  end

  echo "Removing torrents: $torrents"
  tt -t$tline --remove $argv
end
