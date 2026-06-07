#!/bin/sh
# Push the locally-built static blog to public.lomdar.com. Run on the dev box.
# The generator runs locally; this only ships the output.
#   usage: blog-push.sh <generator-output-dir>
set -eu

SRC="${1:?usage: blog-push.sh <site-output-dir>}"
DEST="chris@public.lomdar.com:/var/www/htdocs/lomdar/www/"

# docroot on the server must be writable by the deploy user (chris); see README.
exec rsync -av --delete --chmod=D755,F644 "${SRC%/}/" "$DEST"
