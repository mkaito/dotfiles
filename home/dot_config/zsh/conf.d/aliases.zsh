alias timestamp='date +%s'
alias ts=timestamp

alias cal='cal -m'

# ls shorthands
alias l='ls -Fh --color=auto --group-directories-first'
alias ll='l -lh'
alias la='l -a'
alias lla='la -lh'

# Moving around
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

# Neovim
alias vim='nvim'

# git shorthands
alias g=git
alias g_='g reset'
alias g_h='g reset --hard'
alias g_m='g reset --mixed'
alias g_s='g reset --soft'
alias ga='g add'
alias gap='g add -p'
alias gb='g b'
alias gc-='g co -'
alias gc='g c'
alias gca='g c --amend'
alias gcan='g c --amend --no-edit'
alias gco='g co'
alias gcob='g co -b'
alias gd='g diff'
alias gf='git fixup'
alias gfu='g fetch --all --prune'
alias gl='g l'
alias glp='g l -p'
alias gm='g merge'
alias gm-='gm -'
alias gp='g push'
alias gpp='g push --force-with-lease'
alias gppt='g push --force-with-lease --follow-tags'
alias gpt='g push --follow-tags'
alias gr='g rebase'
alias gra='g rebase --abort'
alias grc='g rebase --continue'
alias gri='g rebase --interactive --autosquash'
alias gs='g status --short'
alias gss='g status'
alias gu='g pull'
alias gz='g stash'
alias gza='g stash apply'
alias gzb='g stash branch'
alias gzc='g stash clear'
alias gzd='g stash drop'
alias gzl='g stash list'
alias gzp='g stash push'
alias gzpu='g stash push --include-untracked'
alias gzs='g stash show --include-untracked'
alias lg='lazygit'

# git commit alias that allows writing oneliners without quoting
alias gcm='noglob gcommit'
gcommit() {
  local opts=()

  while [[ "$1" == -* ]]; do
    opts+=("$1")
    shift
  done

  git commit "${opts[@]}" -m "$*"
}

# gh shorthands
alias ghb='gh browse'
alias ghpv='gh pr view -w'

# Ruby, bundler, rails
alias rake='noglob rake'
alias be='bundle exec'
alias ber='noglob bundle exec rake'

# Misc utilities
function mc {
  command mkdir -p "$1"
  cd "$1"
}

function calculate () { echo "$@" | bc -l ; }
alias calc='noglob calculate'

alias fixperm="find . -type d -exec chmod 755 {} \; && find . -type f -exec chmod 644 {} \;"

# Fetch a .gitignore template: gi python,node
function gi() { curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}

# Yazi — cd to last location on exit
function y() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    builtin cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}

# vim:ft=zsh
