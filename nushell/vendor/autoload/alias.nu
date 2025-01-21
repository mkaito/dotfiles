alias ll = ls -l
alias lla = ll a

# Dev and git
alias vim = nvim
alias be = bundle exec
alias ber = be rake

# Git
alias lg = lazygit
alias g = git
alias ga = g add
alias gap = ga -p
alias gb = g b
alias gc = g c
alias gco = g co
alias gc- = g co -
alias gcob = g cob
alias gd = g d
alias gf = g fetch --all --prune
alias gl = g l
alias gm = g merge
alias gp = g p
alias gpp = g pp
alias gs = g s
alias gss = g ss
alias gu = g pull

# Homebrew
alias b = brew
def bg [] { b upgrade; b cleanup }
alias bs = b search
alias bi = b install
alias br = b remove
