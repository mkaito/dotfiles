# -*- mode: mutt; -*-
set from = christian.hoppner@tweag.io
set folder = imaps://$from@imap.gmail.com:993
set smtp_url = smtp://$from@smtp.gmail.com:587/
set smtp_pass = "$my_tweag_pass"
set imap_pass = "$my_tweag_pass"
set imap_user = $from
set ssl_starttls = yes

mailboxes +INBOX
account-hook $folder 'set imap_pass="$my_tweag_pass" imap_user=christian.hoppner@tweag.io'

set postponed = '+[Gmail]/Drafts'
unset record # Gmail already keeps a copy
set mbox = +Archive/2022
set spoolfile = +INBOX

# vim: ft=muttrc
