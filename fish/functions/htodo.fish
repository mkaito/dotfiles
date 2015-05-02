function htodo
	ack --ignore-dir=playground --ignore-dir=antigen --ignore-dir=jekyll FIXME:\|TODO: ~/dev | sed -e 's|\s\+| |'
end
