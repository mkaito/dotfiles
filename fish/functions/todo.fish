function todo
	ack FIXME:\|TODO: | sed -e 's|\s\+| |'
end
