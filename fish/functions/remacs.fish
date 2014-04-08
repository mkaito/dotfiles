function remacs
	emacsclient -e "(kill-emacs)"
sleep 1
emacs --daemon
end
