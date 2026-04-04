alias b='brew'
alias bs='brew search'
alias bi='brew install'
alias bu='brew update && brew outdated'
alias bg='brew upgrade && brew cleanup'

function br() {
  [[ -z $1 ]] && echo 'No target provided' && return 1
  brew remove "$@"
  brew cleanup
}

# vim:ft=zsh
