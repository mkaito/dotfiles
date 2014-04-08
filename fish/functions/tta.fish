function tta
	if test (count $argv) -eq 1
    tt -a $argv
  else if test (count $argv) -ge 2
    tt -a $argv[1] -w $argv[2..-1]
  end
end
