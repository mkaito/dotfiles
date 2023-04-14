# Nix env/build/shell shorthands/helper functions

# ZSH globbing interferes with flake notation
alias nix='noglob nix'

## Enter nix-shell with zsh
function ns() {
    f="${1:-.}"
    [[ -n $1 ]] && shift
    nix develop "$f" -c zsh "$@"
}

## Enter nix repl
alias nr='nix repl nixpkgs'

## List files in a package, optionally grep it
function nls() {
    [[ -z $1 ]] && return
    find "$(nix build "nixpkgs#$1" --no-link --print-out-paths)"
}

# Local Variables:
# mode: shell-script
# sh-shell: zsh
# End:

# vim:ft=zsh
