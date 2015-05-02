function qdl --description 'Start downloading the queue'
	set -l queue ~/queue.txt

	if test -f $queue
		set -l uri (head -n 1 $queue)
	else
		echo "Queue file not found or empty. Add something to the queue with `qadd <URI>`"
		return 1
	end

	if test -n $uri

		ytdl $uri

		if test $status -eq 0
			sed -i '1d' $queue
		else
			echo "An error has ocurred downloading $uri."
			echo "Line has not been removed from queue!"
		end

	end
end
