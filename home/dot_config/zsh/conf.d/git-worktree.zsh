# Bare-repo + worktree + tmux helpers.
# Layout: <reponame>/<reponame>.git (bare) with sibling <reponame>/<branch>/ worktrees.

_wt_paths() {
  local branch=$1
  if [[ -z $branch ]]; then
    print -u2 "_wt_paths: branch required"
    return 2
  fi

  local common_dir
  common_dir=$(git rev-parse --git-common-dir 2>/dev/null) || {
    print -u2 "_wt_paths: not inside a git repository"
    return 1
  }

  common_dir=${common_dir:A}
  if [[ $(git -C "$common_dir" rev-parse --is-bare-repository 2>/dev/null) != true ]]; then
    print -u2 "_wt_paths: not a bare-repo layout (see wtmig)"
    return 1
  fi

  local repo_root=${common_dir:h}
  print -- "$repo_root"
  print -- "$repo_root/$branch"
  print -- "$common_dir"
}

# Internal: tmux window with given name in current session exists?
_wt_tmux_window_exists() {
  local name=$1
  tmux list-windows -F '#W' 2>/dev/null | grep -Fxq -- "$name"
}

# Internal: symlink overlay files from repo_root into worktree if present.
# Uses absolute targets — survives any worktree depth, broken if repo_root moves.
# Heals broken symlinks at the same names.
_wt_link_overlays() {
  local repo_root=${1:A} worktree=$2
  local -a overlays=(mise.local.toml .mise.local.toml .claude)
  local name src dst
  for name in $overlays; do
    src=$repo_root/$name
    dst=$worktree/$name
    [[ -e $src || -L $src ]] || continue
    if [[ -L $dst && ! -e $dst ]]; then
      rm -- "$dst"
    fi
    if [[ -e $dst || -L $dst ]]; then
      continue
    fi
    ln -s -- "$src" "$dst" \
      && print -- "wt: linked $name" \
      || print -u2 "wt: failed to link $name"
  done
}

# Internal: link harness-visible memory path to canonical shared dir.
# Claude Code derives ~/.claude/projects/<cwd-slug>/memory/ per worktree.
# Symlink each slug's memory to <repo_root>/.claude/memory/ so all worktrees share.
_wt_link_claude_memory() {
  local repo_root=$1 worktree=$2
  local canonical=$repo_root/.claude/memory
  local slug=${worktree//\//-}
  local project_dir=$HOME/.claude/projects/$slug
  local memlink=$project_dir/memory

  mkdir -p -- "$canonical" || {
    print -u2 "wt: failed to create canonical memory dir $canonical"
    return 1
  }
  mkdir -p -- "$project_dir" || {
    print -u2 "wt: failed to create $project_dir"
    return 1
  }

  if [[ -L $memlink ]]; then
    [[ ${memlink:A} == ${canonical:A} ]] && return 0
    print -u2 "wt: $memlink already symlinked elsewhere; leaving alone"
    return 0
  fi
  if [[ -e $memlink ]]; then
    print -u2 "wt: $memlink exists as non-symlink; leaving alone (manual merge needed)"
    return 0
  fi

  ln -s -- "$canonical" "$memlink" \
    && print -- "wt: linked claude memory ($slug → $canonical)" \
    || print -u2 "wt: failed to link claude memory"
}

wtc() {
  emulate -L zsh

  if (( $# < 1 || $# > 2 )); then
    print -u2 "usage: wtc <url> [name]"
    return 2
  fi

  local url=$1 name=$2

  if [[ -z $name ]]; then
    name=${${url##*/}%.git}
  fi

  if [[ -z $name || $name == */* ]]; then
    print -u2 "wtc: could not derive valid name from URL; pass one explicitly"
    return 2
  fi

  if ! command -v git >/dev/null 2>&1; then
    print -u2 "wtc: git not found in PATH"
    return 1
  fi

  local target=$PWD/$name
  local bare=$target/$name.git

  if [[ -e $target ]]; then
    if [[ ! -d $bare ]]; then
      print -u2 "wtc: $target exists but no bare repo at $bare"
      return 1
    fi
    local existing_url
    existing_url=$(git -C "$bare" remote get-url origin 2>/dev/null)
    if [[ $existing_url != $url ]]; then
      print -u2 "wtc: $bare origin url is '$existing_url', not '$url'"
      return 1
    fi
    print -- "wtc: layout exists, skipping clone"
  else
    mkdir -p -- "$target" || {
      print -u2 "wtc: could not create $target"
      return 1
    }
    if ! git clone --bare -- "$url" "$bare"; then
      print -u2 "wtc: clone failed"
      rmdir -- "$target" 2>/dev/null
      return 1
    fi
  fi

  local default
  default=$(git -C "$bare" symbolic-ref --short HEAD 2>/dev/null) || {
    print -u2 "wtc: could not determine default branch"
    return 1
  }

  if [[ $default != main && $default != master && $default != develop ]]; then
    print -u2 "wtc: unsupported default branch: $default (only main/master/develop)"
    return 1
  fi

  local worktree=$target/$default

  if git -C "$bare" worktree list --porcelain | grep -Fxq -- "worktree $worktree"; then
    print -- "wtc: default worktree exists, skipping"
  elif [[ -e $worktree ]]; then
    print -u2 "wtc: $worktree exists but is not a registered worktree"
    return 1
  else
    if ! git -C "$bare" worktree add -- "$worktree" "$default"; then
      print -u2 "wtc: worktree add failed"
      return 1
    fi
  fi

  _wt_link_overlays "$target" "$worktree"
  _wt_link_claude_memory "$target" "$worktree"

  cd -- "$worktree" || return 1

  if [[ -n $TMUX ]]; then
    if _wt_tmux_window_exists "$default"; then
      print -- "wtc: tmux window exists, skipping"
    else
      tmux new-window -n "$default" -c "$worktree"
    fi
  fi
}

wta() {
  emulate -L zsh

  if (( $# != 1 )); then
    print -u2 "usage: wta <branch>"
    return 2
  fi

  local branch=$1

  if [[ $branch == -* ]]; then
    print -u2 "wta: branch name must not start with '-': $branch"
    return 2
  fi
  if [[ $branch == *[[:space:]]* ]]; then
    print -u2 "wta: branch name must not contain whitespace"
    return 2
  fi

  local paths repo_root worktree_path
  local -a path_lines
  paths=$(_wt_paths "$branch") || return $?
  path_lines=(${(f)paths})
  repo_root=${path_lines[1]}
  worktree_path=${path_lines[2]}

  local existing_worktree=""
  existing_worktree=$(git worktree list --porcelain \
    | awk -v b="refs/heads/$branch" '
        /^worktree / { wt = substr($0, 10) }
        $0 == "branch " b { print wt; exit }
      ')

  if [[ -n $existing_worktree ]]; then
    if [[ ${existing_worktree:A} != ${worktree_path:A} ]]; then
      print -u2 "wta: branch '$branch' already checked out at $existing_worktree"
      return 1
    fi
    print -- "wta: worktree exists, skipping"
  elif [[ -e $worktree_path ]]; then
    print -u2 "wta: path exists but is not a registered worktree: $worktree_path"
    return 1
  else
    if git show-ref --verify --quiet -- "refs/heads/$branch"; then
      git worktree add -- "$worktree_path" "$branch" || return 1
    elif git show-ref --verify --quiet -- "refs/remotes/origin/$branch"; then
      git worktree add --track -b "$branch" -- "$worktree_path" "origin/$branch" || return 1
    else
      git worktree add -b "$branch" -- "$worktree_path" || return 1
    fi
  fi

  _wt_link_overlays "$repo_root" "$worktree_path"
  _wt_link_claude_memory "$repo_root" "$worktree_path"

  if [[ -n $TMUX ]]; then
    if _wt_tmux_window_exists "$branch"; then
      print -- "wta: tmux window exists, skipping"
    else
      tmux new-window -n "$branch" -c "$worktree_path"
    fi
  fi
}

wtr() {
  emulate -L zsh

  if (( $# > 1 )); then
    print -u2 "usage: wtr [<worktree-path>]"
    return 2
  fi

  local worktree_path common_dir branch

  if (( $# == 1 )); then
    worktree_path=${1:A}
    if [[ ! -d $worktree_path ]]; then
      print -u2 "wtr: not a directory: $worktree_path"
      return 1
    fi
    common_dir=$(git -C "$worktree_path" rev-parse --git-common-dir 2>/dev/null) || {
      print -u2 "wtr: not a git worktree: $worktree_path"
      return 1
    }
  else
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) != true ]]; then
      print -u2 "wtr: cwd not inside a worktree"
      return 1
    fi
    worktree_path=$(git rev-parse --show-toplevel 2>/dev/null) || {
      print -u2 "wtr: cannot resolve worktree path"
      return 1
    }
    worktree_path=${worktree_path:A}
    common_dir=$(git rev-parse --git-common-dir 2>/dev/null) || {
      print -u2 "wtr: cwd not inside a git repository"
      return 1
    }
  fi
  common_dir=${common_dir:A}

  if [[ $(git -C "$common_dir" rev-parse --is-bare-repository 2>/dev/null) != true ]]; then
    print -u2 "wtr: not a bare-repo layout"
    return 1
  fi

  if ! git -C "$common_dir" worktree list --porcelain | grep -Fxq -- "worktree $worktree_path"; then
    print -u2 "wtr: $worktree_path is not a registered worktree"
    return 1
  fi

  branch=$(git -C "$worktree_path" symbolic-ref --short HEAD 2>/dev/null) || {
    print -u2 "wtr: detached HEAD at $worktree_path"
    return 1
  }

  local default
  default=$(git -C "$common_dir" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||')
  if [[ -z $default ]]; then
    if git -C "$common_dir" show-ref --verify --quiet refs/heads/main; then
      default=main
    elif git -C "$common_dir" show-ref --verify --quiet refs/heads/master; then
      default=master
    elif git -C "$common_dir" show-ref --verify --quiet refs/heads/develop; then
      default=develop
    fi
  fi
  if [[ -n $default && $branch == $default ]]; then
    print -u2 "wtr: refusing to remove default branch '$default'"
    return 1
  fi

  print -- "wtr: remove worktree at $worktree_path"
  local reply
  printf "proceed? [yN] "
  read -r reply
  if [[ $reply != y && $reply != Y && $reply != yes && $reply != YES ]]; then
    print -- "wtr: aborted"
    return 1
  fi

  case $PWD/ in
    $worktree_path/*) cd -- "${common_dir:h}" || return 1 ;;
  esac

  git -C "$common_dir" worktree remove --force -- "$worktree_path" || return 1

  if [[ -n $TMUX ]] && _wt_tmux_window_exists "$branch"; then
    tmux kill-window -t "=$branch"
  fi
}

wtm() {
  emulate -L zsh

  if (( $# != 2 )); then
    print -u2 "usage: wtm <old-branch> <new-branch>"
    return 2
  fi

  local old=$1 new=$2

  if [[ $new == -* ]]; then
    print -u2 "wtm: new branch name must not start with '-': $new"
    return 2
  fi
  if [[ $new == *[[:space:]]* ]]; then
    print -u2 "wtm: new branch name must not contain whitespace"
    return 2
  fi
  if [[ $old == $new ]]; then
    print -u2 "wtm: old and new names are identical"
    return 2
  fi

  local paths repo_root old_path new_path
  local -a path_lines
  paths=$(_wt_paths "$old") || return $?
  path_lines=(${(f)paths})
  repo_root=${path_lines[1]}
  old_path=${path_lines[2]}
  new_path=$repo_root/$new

  local old_wt=0 new_wt=0 old_br=0 new_br=0 old_win=0 new_win=0
  local wt_list
  wt_list=$(git worktree list --porcelain)
  print -r -- "$wt_list" | grep -Fxq -- "worktree $old_path" && old_wt=1
  print -r -- "$wt_list" | grep -Fxq -- "worktree $new_path" && new_wt=1
  git show-ref --verify --quiet -- "refs/heads/$old" && old_br=1
  git show-ref --verify --quiet -- "refs/heads/$new" && new_br=1
  if [[ -n $TMUX ]]; then
    _wt_tmux_window_exists "$old" && old_win=1
    _wt_tmux_window_exists "$new" && new_win=1
  fi

  if (( old_wt && new_wt )); then
    print -u2 "wtm: worktrees registered at both $old_path and $new_path"
    return 1
  fi
  if (( old_br && new_br )); then
    print -u2 "wtm: branches '$old' and '$new' both exist"
    return 1
  fi
  if (( !old_wt && !new_wt && !old_br && !new_br )); then
    print -u2 "wtm: neither '$old' nor '$new' present"
    return 1
  fi

  if (( !old_wt && !new_wt )) && [[ -e $new_path || -L $new_path ]]; then
    print -u2 "wtm: $new_path exists but is not a registered worktree"
    return 1
  fi
  if (( old_wt )) && [[ -e $new_path || -L $new_path ]]; then
    print -u2 "wtm: target path already exists: $new_path"
    return 1
  fi

  local was_inside_old=0
  case $PWD/ in
    $old_path/*) was_inside_old=1 ;;
  esac

  if (( old_br )); then
    git branch -m -- "$old" "$new" || return 1
  fi

  if (( old_wt )); then
    if ! git worktree move -- "$old_path" "$new_path"; then
      print -u2 "wtm: worktree move failed"
      return 1
    fi
  fi

  if (( was_inside_old )); then
    local rel=${PWD#$old_path}
    cd -- "$new_path$rel" || cd -- "$new_path" || return 1
  fi

  _wt_link_claude_memory "$repo_root" "$new_path"

  if [[ -n $TMUX ]]; then
    if (( old_win && new_win )); then
      print -u2 "wtm: tmux windows '$old' and '$new' both exist; leaving alone"
    elif (( old_win )); then
      tmux rename-window -t "=$old" "$new"
    fi

    if (( old_wt )) && _wt_tmux_window_exists "$new"; then
      local pane_id pane_path rel target_dir
      tmux list-panes -t "=$new" -F '#{pane_id} #{pane_current_path}' 2>/dev/null \
        | while IFS=' ' read -r pane_id pane_path; do
            case $pane_path/ in
              $old_path/*)
                rel=${pane_path#$old_path}
                target_dir=$new_path$rel
                tmux send-keys -t "$pane_id" "cd ${(q)target_dir}" Enter
                ;;
            esac
          done
    fi
  fi
}

wtmig() {
  emulate -L zsh

  # Must run from inside a regular (non-bare) clone, at its top level.
  local toplevel
  toplevel=$(git rev-parse --show-toplevel 2>/dev/null) || {
    print -u2 "wtmig: not inside a git repository"
    return 1
  }
  toplevel=${toplevel:A}
  if [[ ${PWD:A} != $toplevel ]]; then
    print -u2 "wtmig: must run from clone root: $toplevel"
    return 1
  fi

  if [[ $(git rev-parse --is-bare-repository 2>/dev/null) == true ]]; then
    print -u2 "wtmig: already a bare repo"
    return 1
  fi

  local common_dir expected
  common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
  common_dir=${common_dir:A}
  expected=$toplevel/.git
  if [[ $common_dir != ${expected:A} ]]; then
    print -u2 "wtmig: expected ${expected:A}, got $common_dir (worktree or unusual layout)"
    return 1
  fi

  if [[ -n $(git status --porcelain 2>/dev/null) ]]; then
    print -u2 "wtmig: uncommitted changes; stash or commit first"
    return 1
  fi

  local current
  current=$(git symbolic-ref --short HEAD 2>/dev/null) || {
    print -u2 "wtmig: HEAD detached; check out default branch first"
    return 1
  }

  local default
  default=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||')
  if [[ -z $default ]]; then
    if git show-ref --verify --quiet refs/heads/main; then
      default=main
    elif git show-ref --verify --quiet refs/heads/master; then
      default=master
    elif git show-ref --verify --quiet refs/heads/develop; then
      default=develop
    else
      print -u2 "wtmig: could not determine default branch"
      return 1
    fi
  fi
  if [[ $default != main && $default != master && $default != develop ]]; then
    print -u2 "wtmig: unsupported default branch: $default (only main/master/develop)"
    return 1
  fi
  if [[ $current != $default ]]; then
    print -u2 "wtmig: current branch is '$current'; switch to '$default' first"
    return 1
  fi

  local name=${toplevel:t}
  local parent=${toplevel:h}
  if [[ -e $parent/$name.tmp ]]; then
    print -u2 "wtmig: $parent/$name.tmp already exists"
    return 1
  fi

  if ! command -v rsync >/dev/null 2>&1; then
    print -u2 "wtmig: rsync not found in PATH"
    return 1
  fi

  local -a other_branches
  other_branches=(${(f)"$(git for-each-ref --format='%(refname:short)' refs/heads 2>/dev/null)"})
  other_branches=(${other_branches:#$default})

  local -a overlays_found
  local f
  for f in mise.local.toml .mise.local.toml .claude; do
    [[ -e $toplevel/$f ]] && overlays_found+=("$f")
  done

  local old_slug=${toplevel//\//-}
  local mem_src=$HOME/.claude/projects/$old_slug/memory
  local has_memory=0
  [[ -d $mem_src ]] && has_memory=1

  print -- "wtmig plan:"
  print -- "  parent:        $parent"
  print -- "  repo name:     $name"
  print -- "  default:       $default"
  print -- "  new layout:    $parent/$name/$name.git (bare) + $parent/$name/$default/ (worktree)"
  if (( ${#overlays_found} )); then
    print -- "  overlays:      ${(j:, :)overlays_found} -> lifted to $name/, symlinked into $default/"
  else
    print -- "  overlays:      (none found)"
  fi
  if (( has_memory )); then
    print -- "  claude memory: $mem_src -> $name/.claude/memory"
  else
    print -- "  claude memory: (none found)"
  fi
  if (( ${#other_branches} )); then
    print -- "  extra wtas:    ${(j:, :)other_branches}"
  else
    print -- "  extra wtas:    (none)"
  fi
  print

  local reply
  printf "proceed? [yN] "
  read -r reply
  if [[ $reply != y && $reply != Y && $reply != yes && $reply != YES ]]; then
    print -- "wtmig: aborted"
    return 1
  fi

  cd -- "$parent" || return 1
  mv -- "$name" "$name.tmp" || return 1
  mkdir -- "$name" || return 1
  mv -- "$name.tmp/.git" "$name/$name.git" || return 1
  git -C "$name/$name.git" config core.bare true || return 1
  git -C "$name/$name.git" config --unset core.worktree 2>/dev/null

  local default_path=$parent/$name/$default
  if ! git -C "$parent/$name/$name.git" worktree add -- "$default_path" "$default"; then
    print -u2 "wtmig: worktree add failed"
    return 1
  fi

  if ! rsync -a --exclude=.git -- "$parent/$name.tmp/" "$default_path/"; then
    print -u2 "wtmig: rsync failed"
    return 1
  fi
  git -C "$default_path" update-index --refresh >/dev/null 2>&1

  for f in $overlays_found; do
    local src=$default_path/$f
    [[ -e $src ]] || continue
    mv -- "$src" "$parent/$name/$f" || return 1
    ln -s -- "../$f" "$src" || return 1
  done

  if (( has_memory )) && [[ ! -e $parent/$name/.claude/memory ]]; then
    mkdir -p -- "$parent/$name/.claude" || return 1
    mv -- "$mem_src" "$parent/$name/.claude/memory" || return 1
  fi

  cd -- "$default_path" || return 1
  _wt_link_claude_memory "$parent/$name" "$default_path"

  if [[ -n $TMUX ]]; then
    if _wt_tmux_window_exists "$default"; then
      print -- "wtmig: tmux window '$default' exists, skipping"
    else
      tmux new-window -n "$default" -c "$default_path"
    fi
  fi

  local b
  for b in $other_branches; do
    wta "$b" || print -u2 "wtmig: wta $b failed; continuing"
  done

  rm -rf -- "$parent/$name.tmp" || {
    print -u2 "wtmig: cleanup failed for $parent/$name.tmp"
    return 1
  }

  print -- "wtmig: done. cwd: $PWD"
}

# --- completion -------------------------------------------------------------

# Local heads + origin remotes (origin/ stripped). For wta.
_wta_complete() {
  local -a locals remotes branches
  locals=(${(f)"$(git for-each-ref --format='%(refname:short)' refs/heads 2>/dev/null)"})
  remotes=(${(f)"$(git for-each-ref --format='%(refname:short)' refs/remotes/origin 2>/dev/null \
    | sed -n 's|^origin/||p')"})
  remotes=(${remotes:#HEAD})
  branches=(${(u)locals} ${(u)remotes})
  _describe -t branches 'branch' branches
}

# Registered worktree paths (excluding bare). For wtr.
_wtr_complete() {
  local -a paths
  paths=(${(f)"$(git worktree list --porcelain 2>/dev/null \
    | awk '/^worktree / { wt = substr($0, 10); bare = 0 }
           /^bare$/      { bare = 1 }
           /^branch /    { if (!bare) print wt }')"})
  _describe -t paths 'worktree path' paths
}

# Branches with registered worktrees. For wtm first arg.
_wtm_complete() {
  if (( CURRENT == 2 )); then
    local -a branches
    branches=(${(f)"$(git worktree list --porcelain 2>/dev/null \
      | sed -n 's|^branch refs/heads/||p')"})
    _describe -t branches 'worktree branch' branches
  fi
}

(( ${+functions[compdef]} )) && {
  compdef _wta_complete wta
  compdef _wtr_complete wtr
  compdef _wtm_complete wtm
}

# vim:ft=zsh
