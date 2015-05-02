function qadd --description 'Add a line to the download queue' --argument uri path
	set -l queue ~/queue.txt

	if test -f $queue
		echo "$uri $path" >> $queue
	else
		echo "$uri $path" > $queue
	end

end
