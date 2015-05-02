function qedit --description 'Edit the queue file with a text editor'
	set -l queue ~/queue.txt

	if test -f $queue
		eval $EDITOR $queue
	else
		echo "The queue file does not exist. Add something first with `qadd <URI>`"
	end

end
