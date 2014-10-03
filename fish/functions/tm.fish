function tm --description 'tmux attach or create session $target' --argument target
	
	# If no target was given, default to the current parent folder's name
	if test -z $target
		set target (basename $PWD)
	end

	# Check if a session already exists, and if it does, attach. Otherwise, create, then attach.
	if tmux has-session -t $target
		tmux attach -t $target
	else
		tmux new-session -s $target
	end

end
