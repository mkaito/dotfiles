# env.nu
#
# Installed by:
# version = "0.101.0"
#
# Previously, environment variables were typically configured in `env.nu`.
# In general, most configuration can and should be performed in `config.nu`
# or one of the autoload directories.
#
# This file is generated for backwards compatibility for now.
# It is loaded before config.nu and login.nu
#
# See https://www.nushell.sh/book/configuration.html
#
# Also see `help config env` for more options.
#
# You can remove these comments if you want or leave
# them for future reference.

use std/util "path add"

mkdir ($nu.data-dir | path join "vendor/autoload")
mkdir ($nu.data-dir | path join "vendor/use")

path add "/usr/local/bin"
path add "/opt/homebrew/bin"
# path add ($env.CARGO_HOME | path join "bin")
path add "~/.local/bin"
path add "~/dev/dotfiles/bin"

# Mise
let mise_path = $nu.default-config-dir | path join "vendor/use/mise.nu"
^mise activate nu | save $mise_path --force

# argc-completions
$env.ARGC_COMPLETIONS_ROOT = '/Users/mkaito/dev/build/argc-completions'
$env.ARGC_COMPLETIONS_PATH = ($env.ARGC_COMPLETIONS_ROOT + '/completions/macos:' + $env.ARGC_COMPLETIONS_ROOT + '/completions')
$env.PATH = ($env.PATH | prepend ($env.ARGC_COMPLETIONS_ROOT + '/bin'))
