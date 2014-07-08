set -U fish_greeting

if status -l
  set -x PAGER less
  set -x EDITOR vim
  set -x ALTERNATE_EDITOR
  set -x BROWSER firefox
  set -x XERM ~/dev/bin/terminal
  set -x TERMINAL $XTERM
  set -x PERL_SIGNALS unsafe
  set -x WINEDEBUG -all
  set -x GOPATH ~/dev/go
  set -x LS_COLORS $LS_COLORS':ex=32:di=34'
	set -x CFLAGS "-std=gnu11 -g -Wall"

  set -x PYENV_ROOT ~/dev/build/pyenv
  source $PYENV_ROOT/versions/2.7.6/bin/virtualenvwrappper.sh
  eval (pyenv init -)
  eval (rbenv init -)

  set -x PATH ~/dev/bin $GOPATH/bin $PYENV_ROOT/bin ~/perl5/bin ~/.cabal/bin /opt/android-sdk/platform-tools $PATH

  set -x ANDROIDSDK /opt/android-sdk
  set -x ANDROIDNDK /opt/android-ndk
  set -x ANDROIDNDKVER r8
  set -x ANDROIDAPI 8

  set -x LEDGER_FILE ~/ledger/personal.ledger

  set -x PERL_LOCAL_LIB_ROOT ~/perl5
  set -x PERL_MB_OPT "--install_base $HOME/perl5"
  set -x PERL_MM_OPT INSTALL_BASE=$HOME/perl5
  set -x PERL5LIB "$HOME/perl5/lib/perl5/x86_64-linux-thread-multi:$HOME/perl5/lib/perl5"
end
