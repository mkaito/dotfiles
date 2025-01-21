# argc-completions
$env.ARGC_COMPLETIONS_ROOT = '/Users/mkaito/dev/build/argc-completions'
$env.ARGC_COMPLETIONS_PATH = ($env.ARGC_COMPLETIONS_ROOT + '/completions/macos:' + $env.ARGC_COMPLETIONS_ROOT + '/completions')
$env.PATH = ($env.PATH | prepend ($env.ARGC_COMPLETIONS_ROOT + '/bin'))

let zoxide_completer = {|spans|
    $spans | skip 1 | zoxide query -l ...$in | lines | where {|x| $x != $env.PWD}
}

def _argc_completer [args: list<string>] {
    argc --argc-compgen nushell "" ...$args
        | split row "\n"
        | each { |line| $line | split column "\t" value description }
        | flatten
}

let argc_completer = {|spans|
    _argc_completer $spans
}

let external_completer = {|spans|
    # if the current command is an alias, get it's expansion
    let expanded_alias = (scope aliases | where name == $spans.0 | get -i 0 | get -i expansion)

    # overwrite
    let spans = (if $expanded_alias != null  {
        # put the first word of the expanded alias first in the span
        $spans | skip 1 | prepend ($expanded_alias | split row " " | take 1)
    } else { $spans })

    match $spans.0 {
        __zoxide_z | __zoxide_zi => $zoxide_completer
        _ => $argc_completer
    } | do $in $spans
}

$env.config.completions.external = {
    enable: true
    completer: $external_completer
}
