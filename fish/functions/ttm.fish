function ttm
	set -l torrents
  set -l tline
  set -l target

  if not isatty 0

    # Need at least one argument: the target path
    if test (count $argv) -ge 1
      while read line
        set torrents $torrents (echo $line | grep -Po '^\s*\d+' | tr -d ' ')
      end

      # Makeshift hacky version of $torrents.join(',')
      set tline $torrents[1]
      if test (count $torrents) -ge 2
        for t in $torrents[2..-1]
          set tline $tline","$t
        end
      end

      set target (realpath $argv[1])

      # Slice the arguments to get rid of those we used
      # and so we have a clean args array to pass down to tt.
      if test (count $argv) -ge 2
        set argv $argv[2..-1]
      else
        set -e argv
      end

    else
      echo "Insufficient parameters."
      return 65
    end


  else

    # Need two parameters:
    #  a string "2,3,4,5", passed to transmission-remote as argument to -t
    #  a target path
    if test (count $argv) -ge 2
      set tline $argv[1]
      set target (realpath $argv[2])

      # Slice the arguments to get rid of those we used
      # and so we have a clean args array to pass down to tt.
      if test (count $argv) -ge 3
        set argv $argv[3..-1]
      else
        set -e argv
      end

    else
      echo "Insufficient parameters."
      return 65
    end
  end

  echo "Moving torrents: $torrents"
  echo "Target: $target"
  tt -t$tline --move $target $argv
end
