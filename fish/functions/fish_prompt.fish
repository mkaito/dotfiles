# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch "#666666"
set __fish_git_prompt_char_upstream_prefix ""
set __fish_git_prompt_char_stateseparator ""
set fish_color_error "red"

# Status Chars
# set __fish_git_prompt_char_dirtystate '⚡'
# set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_stashstate '↩'
# set __fish_git_prompt_char_upstream_ahead '↑'
# set __fish_git_prompt_char_upstream_behind '↓'

function fish_prompt --description 'Write out the prompt'

  set -l last_status $status

  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end
  
  # PWD
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal

  # Git branch and status
  printf '%s' (__fish_git_prompt ' %s')

  # Long execution times
  if test -n $CMD_DURATION
    set_color $fish_color_error
    echo -n " "$CMD_DURATION
    set_color normal
  end

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  printf "\n❯ "

end
