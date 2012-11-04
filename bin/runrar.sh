function recursive_unrar() {
	for f in **/*.rar(N); do
		local cwd=`pwd` fbn=${f##*/}
		cd "${f%/*}"
		unrar e "$fbn"
		rm "${fbn%%.*}.r{01..99} $fbn"
		cd "$cwd"
	done
}

recursive_unrar
