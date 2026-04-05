# dotfiles

Gentoo Linux and macOS configs, managed with [chezmoi](https://chezmoi.io) and [mise](https://mise.jdx.dev).

## Structure

```
home/           chezmoi source — deploys to $HOME
system/gentoo/  Gentoo system config (/etc) and local portage overlay
.mise/tasks/    task runner scripts
vendor/         git submodules (zgenom, tokyonight.nvim)
```

## Bootstrap

```sh
./bootstrap.sh
```

Installs mise. macOS homebrew installation is not yet automated — see the TODO in `bootstrap.sh`.

## Dotfiles

```sh
mise run install   # first run: init submodules, link chezmoi source, apply
chezmoi apply      # re-apply after changes
```

## Gentoo system config

Only runs on aldheim.

```sh
mise run gentoo:install   # deploy repo -> /etc
mise run gentoo:pull      # pull live /etc changes back into repo
mise run gentoo:diff      # diff repo vs live
```

## Packages

`chezmoi apply` installs missing packages via `emerge` or `brew` when `.chezmoidata/packages.yaml` changes.

## Platform-specific files

Exclusions are in `home/.chezmoiignore`. Platform-specific zsh config is in `home/dot_config/zsh/conf.d/`: `linux.zsh`, `gentoo.zsh`, `aldheim.zsh`, `macos.zsh`.
