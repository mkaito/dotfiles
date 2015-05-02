function fish_prompt --description 'Write out the prompt'
	set -l last_status $status

  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end

  # PWD
  set_color cyan
  echo -n (prompt_pwd)
  set_color normal

  # Git branch and status
  printf '%s' (__fish_git_prompt ' %s')

	# Long execution times, only shown if last command ran for a second or more.
	# TODO: Convert to seconds and minutes as number grows.
  if test \( -n $CMD_DURATION \) -a \( $CMD_DURATION -ge 1000 \)
    set_color $fish_color_error
    echo -n " "$CMD_DURATION"ms"
    set_color normal
  end

  if test $last_status -ne 0
    set_color red
  else
    set_color green
  end

  printf "\n❯ "
end
