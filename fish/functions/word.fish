function word --argument quantity
	if test -z $quantity
		set quantity 1
	end

	shuf -n $quantity /usr/share/dict/words
end
