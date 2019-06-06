# Nix env/build/shell shorthands/helper functions

## Edit configuration.nix and rebuild the system
alias conf='vim ~/dev/dotfiles/nix/configuration.nix; sudo nixos-rebuild -I nixpkgs=$HOME/dev/nixpkgs switch'

## Enter nix-shell with zsh
alias ns='nix-shell --run "zsh"'

## Enter nix repl with nixpkgs loaded
alias nr='nix repl "<nixpkgs>"'

## Grep the list of available packages
function nss() {
    local cachef="$HOME/.cache/nixpkgs"
    if [[ ! $(find "$cachef" -mtime -1 -print) ]]; then
        rm -f "$cachef"
        nix-env -qaP > "$cachef"
    fi

    grep -i "$1" "$cachef"
}

## List files in a package, optionally grep it
function nls() {

    if [[ ! -z $1 ]]; then
        fd "${2:-.}" "$(nix-build '<nixpkgs>' -A "$1" --no-out-link)"
    else
        cat <<-EOF
			nls: List files in a nix package, optionally grep the output

			Usage:
			nls <nixpkgs attribute> [grep]
			EOF
    fi
}

# Create a boilerplate .envrc and shell.nix
nixify() {
  if [ ! -e ./.envrc ]; then
    echo "use nix" > .envrc
    direnv allow
  fi
  if [ ! -e shell.nix ]; then
    cat > shell.nix <<'EOF'
with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    zsh
  ];
}
EOF
    ${EDITOR:-vim} default.nix
  fi
}

# Local Variables:
# mode: shell-script
# sh-shell: zsh
# End:

# vim:ft=zsh
