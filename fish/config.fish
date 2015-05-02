# Disable greeting message on load
set -U fish_greeting

# Set up pyenv
set -x PATH "$HOME/.pyenv/bin" $PATH
status --is-interactive; and . (pyenv init -|psub)
status --is-interactive; and . (pyenv virtualenv-init -|psub)

# Colorize man pages
set -xU LESS_TERMCAP_mb (printf "\e[01;31m")      # begin blinking
set -xU LESS_TERMCAP_md (printf "\e[01;31m")      # begin bold
set -xU LESS_TERMCAP_me (printf "\e[0m")          # end mode
set -xU LESS_TERMCAP_se (printf "\e[0m")          # end standout-mode
set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")   # begin standout-mode - info box
set -xU LESS_TERMCAP_ue (printf "\e[0m")          # end underline
set -xU LESS_TERMCAP_us (printf "\e[01;32m")      # begin underline

# Miscelaneous abbreviations
abbr -a yas='yaourt -S'
abbr -a yass='yaourt -Ss'
abbr -a yard='yaourt -Rd'
abbr -a yasyu='yaourt -Syu'
abbr -a yasyy='yaourt -Syy'
abbr -a ss='sudo systemctl'
abbr -a ssu='systemctl --user'
abbr -a ec='emacsclient -c -n'
abbr -a e='emacsclient -n'
abbr -a ect='emacsclient -nw'
abbr -a p='pass -c'
abbr -a ll='ls -lh'
abbr -a lla='ll -a'
abbr -a pacleandeps='sudo pacman -Rns (pacman -Qdtq)'
abbr -a be='bundle exec'
abbr -a be='bundle exec rake'
