# dotfiles

Gentoo Linux and macOS configs, managed with [chezmoi](https://chezmoi.io) and [mise](https://mise.jdx.dev).

## Structure

```
home/             chezmoi source — deploys to $HOME
system/           3-tier system config (/etc), see below
  hosts/<name>    per-host manifest: os= role=
  lib/deploy.sh   layered deploy/pull/diff core (POSIX sh)
  <os>/           bootstrap.sh, rsync-filter, <role>/etc (tier 2), <host>/etc (tier 3)
.mise/tasks/      task runner scripts
vendor/           git submodules (zgenom, tokyonight.nvim)
```

The mkaito portage overlay lives in its own repo at ~/dev/portage.

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

## System config (3 tiers)

A machine composes from three explicit, separately-installed tiers, resolved
from `system/hosts/$(hostname)` (`os=`, `role=`):

1. **bootstrap** — bare-metal prep for the OS (`system/<os>/bootstrap.sh`).
2. **common** — OS/role baseline (`system/<os>/<role>/etc`), e.g. `gentoo/desktop`.
3. **machine** — host-specific deltas (`system/<os>/<host>/etc`), overlaid on common.

```sh
mise run system:bootstrap   # tier 1
mise run system:common      # tier 2 -> /etc
mise run system:machine     # tier 3 -> /etc
mise run system:pull        # pull live /etc changes back into the owning tier
mise run system:diff        # diff repo tiers vs live /etc
```

Where mise is unavailable (e.g. OpenBSD), call the core directly:
`sh system/lib/deploy.sh {bootstrap|common|machine|pull|diff}`.

Deploy is sequential per layer with no `--delete`: machine overlays common, and
file *removals* are not auto-propagated — `system:diff` surfaces drift, prune
`/etc` by hand. `make.conf` is a directory (`make.conf/00-base` + host
`make.conf/50-host`) so the layers don't clobber each other.

## Packages

`chezmoi apply` installs missing packages via `emerge` or `brew` when `.chezmoidata/packages.yaml` changes.

## Platform-specific files

Exclusions are in `home/.chezmoiignore`. Platform-specific zsh config is in `home/dot_config/zsh/conf.d/`: `linux.zsh`, `gentoo.zsh`, `aldheim.zsh`, `macos.zsh`.
