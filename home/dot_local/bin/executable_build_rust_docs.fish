#!/usr/bin/env fish
# Update the repo at $SRC, build the docs, convert epubs to mobi, and put both in $DEST, git commit, git push.

set -l src $HOME/dev/build/rust
set -l dest $HOME/dev/build/rustdocs_ebook

if not test -d $src -a -d $dest
	echo "Please make sure these directories exist:"
	echo "Source: $src"
	echo "Destination: $dest"
	exit 127
end

cd $src
git fetch origin
git reset --hard origin/master

./configure
make docs

if test $status -eq 0
	cd doc
	for f in *.epub
		kindlegen $f -c2
	end

	mv -u -t $dest/ *.epub
	mv -u -t $dest/ *.mobi

	cd $dest
	git add *
	git commit -m "Nightly docs: "(date +%c)
	git push origin master
	exit $status
else
	exit $status
end
