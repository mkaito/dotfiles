#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3
# -*- coding: utf-8 -*-

# Based on:
# https://docs.python.org/3/library/email-examples.html

import sys
import os
import tempfile
import mimetypes
import webbrowser
from email import policy
from email.parser import BytesParser


raw = sys.stdin.buffer.read()

if not os.isatty(0):
    fd = os.open('/dev/tty', os.O_RDONLY)
    if fd < 0:
        sys.stderr.write('Unable to open an input tty.\n')
        sys.exit(-1)
    else:
        os.dup2(fd, 0)
        os.close(fd)

msg = BytesParser(policy=policy.default).parsebytes(raw)

# We can extract the richest alternative in order to display it:
richest = msg.get_body()
partfiles = {}
if richest['content-type'].maintype == 'text':
    if richest['content-type'].subtype == 'plain':
        for line in richest.get_content().splitlines():
            print(line)
        sys.exit()
    elif richest['content-type'].subtype == 'html':
        body = richest
    else:
        print("Don't know how to display {}".format(richest.get_content_type()))
        sys.exit()
elif richest['content-type'].content_type == 'multipart/related':
    body = richest.get_body(preferencelist=('html'))
    for part in richest.iter_attachments():
        fn = part.get_filename()
        if fn:
            extension = os.path.splitext(part.get_filename())[1]
        else:
            extension = mimetypes.guess_extension(part.get_content_type())
        with tempfile.NamedTemporaryFile(suffix=extension, delete=False) as f:
            f.write(part.get_content())
            # again strip the <> to go from email form of cid to html form.
            partfiles[part['content-id'][1:-1]] = f.name
else:
    print("Don't know how to display {}".format(richest.get_content_type()))
    sys.exit()
with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix=".html") as f:
    # The magic_html_parser has to rewrite the href="cid:...." attributes to
    # point to the filenames in partfiles.  It also has to do a safety-sanitize
    # of the html.  It could be written using html.parser.
    template = """
    <!DOCTYPE html><html>
    <head><meta charset="utf-8"></head>
    <body>{}</body>
    </html>
    """
    f.write(template.format(body.get_content()))
webbrowser.open(f.name)
# os.remove(f.name)
for fn in partfiles.values():
    os.remove(fn)
