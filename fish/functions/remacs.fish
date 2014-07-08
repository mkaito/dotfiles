function remacs
	emacsclient -e "(kill-emacs)"
	sleep 1
	pkill emacs
	emacs --daemon
end
