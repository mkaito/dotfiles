# -*- mode: mutt; -*-
set from = chris@mkaito.com
set folder = imap://chris@mkaito.com:143
set smtp_url = smtp://chris@mkaito.com:25/
set smtp_pass = "$my_legacy_pass"
set imap_pass = "$my_legacy_pass"
set imap_user = chris
unset ssl_starttls

mailboxes +INBOX
account-hook $folder 'set imap_pass="$my_legacy_pass" imap_user=chris'

set postponed = +Drafts
set record = +Sent
set mbox = +Archive/2022
set spoolfile = +INBOX

# vim: ft=muttrc
