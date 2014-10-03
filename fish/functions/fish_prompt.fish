function fish_prompt --description 'Write out the prompt'
	
  set -l last_status $status
	# set -l tmux_active_session (tmux ls | grep attached | sed -e 's/\([^:]\+\):.*/\1/')
	# set -l tmux_active_window (tmux lsw | grep \* | sed -e 's/[^ ]\+ \([^\*]\+\)\*.*/\1/')

  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end
  
  # PWD
  set_color cyan
  echo -n (prompt_pwd)
  set_color normal

  # Git branch and status
  printf '%s' (__fish_git_prompt ' %s')

	# Print tmux status, if available
	# if test -n $tmux_active_session
	# 	set_color green

	# 	echo -n " $tmux_active_session"

	# 	## FIXME: This will print the window name even outside a tmux session.
	# 	# if test -n $tmux_active_window
	# 	# 	echo -n "/$tmux_active_window"
	# 	# end

	# 	set_color normal
	# end

  # Long execution times
  # if test -n $CMD_DURATION
  #   set_color $fish_color_error
  #   echo -n " "$CMD_DURATION
  #   set_color normal
  # end

  if not test $last_status -eq 0
    set_color red
  else
    set_color green
  end

  printf "\n❯ "

end
