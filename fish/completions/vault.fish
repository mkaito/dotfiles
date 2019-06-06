
function __complete_vault
    set -lx COMP_LINE (string join ' ' (commandline -o))
    test (commandline -ct) = ""
    and set COMP_LINE "$COMP_LINE "
    /nix/store/kkqkwgv5p3fjyz7rfrvvi3a3a127nxmf-vault-0.9.5/bin/vault
end
complete -c vault -a "(__complete_vault)"

