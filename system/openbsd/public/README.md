# public.lomdar.com — OpenBSD host runbook

Single OpenBSD box serving:
- **static blog** at `https://www.lomdar.com/` (built on the dev box, rsync'd up)
- **Forgejo** at `https://git.lomdar.com/`

This dir is the source of truth for the box's config. The `etc/` trees are
deployed by the shared `system/lib/deploy.sh` (tiers `openbsd/server` +
`openbsd/public`); the phases below are the one-time setup procedure, including
the hand-copies needed before rsync exists on a blank box. Secrets (TLS keys,
real `app.ini`) live only on the box, never in this repo.

The forgejo OpenBSD port lives in its own repo (`~/dev/ports`); build a binary
package there and `pkg_add` it on the box — no ports tree on the server.

## Architecture

One public IP, so one daemon owns `:443`. Forgejo needs TLS **and** a reverse
proxy; `httpd` can't proxy, so `relayd` owns `:443`.

```
:80   httpd          ACME http-01 challenges + 301 -> https
127.0.0.1:8080 httpd static blog docroot (plain, localhost only)
:443  relayd         TLS termination (acme certs), route by Host:
                       www.lomdar.com -> 127.0.0.1:8080  (httpd static)
                       git.lomdar.com -> 127.0.0.1:3000  (forgejo)
127.0.0.1:3000 forgejo   SQLite, behind relayd
```

## Repo -> server file map

| repo path                         | server path                         |
|-----------------------------------|-------------------------------------|
| `etc/pf.conf`                     | `/etc/pf.conf`                      |
| `etc/httpd.conf`                  | `/etc/httpd.conf`                   |
| `etc/relayd.conf`                 | `/etc/relayd.conf`                  |
| `etc/acme-client.conf`            | `/etc/acme-client.conf`            |
| `etc/forgejo/app.ini.example`     | `/etc/forgejo/app.ini` (fill secrets) |
| `../server/etc/doas.conf`         | `/etc/doas.conf` (tier 2)           |
| `../server/etc/rc.conf.local`     | reference for `rcctl enable` lines  |
| forgejo port (`~/dev/ports`)      | build pkg, `pkg_add` on the box     |
| `deploy/blog-push.sh`             | runs on the dev box                 |

(`doas.conf` and `rc.conf.local` are shared baseline, so they live in the
`openbsd/server` tier; everything else is host-specific.)

---

## Phase 0 — Console bootstrap (VNC root shell, blank box)

Goal: reach a locked-down, SSH-reachable box. **Keep the VNC session open until
SSH is verified** so a mistake can't lock you out.

1. Network sanity: `ifconfig`, `route -n show`, `cat /etc/resolv.conf`, ping out.
   Confirm `/etc/myname` = `public.lomdar.com`.
2. Mirror: set `/etc/installurl` to `https://cdn.openbsd.org/pub/OpenBSD`.
3. Admin user (verify the `wheel` group exists: `getent group wheel`):

       useradd -m -G wheel chris
       passwd chris
       mkdir -p ~chris/.ssh && chmod 700 ~chris/.ssh
       # paste your pubkey:
       vi ~chris/.ssh/authorized_keys && chmod 600 ~chris/.ssh/authorized_keys
       chown -R chris:chris ~chris/.ssh

4. `doas`: copy `../server/etc/doas.conf` -> `/etc/doas.conf` (`permit persist keepenv :wheel`).
5. Harden sshd in `/etc/ssh/sshd_config`: `PermitRootLogin no`,
   `PasswordAuthentication no`, `PubkeyAuthentication yes`. Then
   `rcctl restart sshd`. **From the dev box**, confirm `ssh chris@public.lomdar.com`
   works and `ssh root@...` is refused before closing VNC.
6. Firewall: copy `etc/pf.conf` -> `/etc/pf.conf`, then
   `pfctl -nf /etc/pf.conf` (syntax check) and `pfctl -f /etc/pf.conf`.
   `pf=YES` is the default; confirm with `rcctl get pf` / `pfctl -sr`.
7. Upgrade 7.8 -> 7.9 and patch:

       sysupgrade            # downloads, reboots into the upgrade automatically
       # reconnect, then:
       syspatch && pkg_add -u && reboot
       uname -r              # expect 7.9

---

## Phase 1 — Build & install the Forgejo port

Forgejo is not in ports and ships no OpenBSD binary, so we package it as a local
port, kept in its own repo (`~/dev/ports`, `www/forgejo/`). **Build it in a local
OpenBSD 7.9/amd64 VM, not on this box** — `/usr` here is only ~2.7G, too small for
the ports tree + go toolchain + Go build cache. Full build procedure lives with
the port in `~/dev/ports`.

Result: a `forgejo-<ver>.tgz`. Copy it to the box and install:

    scp forgejo-15.0.2.tgz chris@public.lomdar.com:/tmp/
    doas pkg_add /tmp/forgejo-15.0.2.tgz

`pkg_add` needs no ports tree. The package creates the `_forgejo` user (uid/gid
950), installs `/usr/local/sbin/forgejo`, static assets under
`/usr/local/share/forgejo`, and the rc script. git is pulled as a dependency.

---

## Phase 2 — Certs & services

1. Data/config dirs and secrets:

       doas install -d -o _forgejo -g _forgejo -m 700 /var/forgejo
       doas install -d -o _forgejo -g _forgejo -m 750 /etc/forgejo
       doas -u _forgejo /usr/local/sbin/forgejo generate secret SECRET_KEY
       doas -u _forgejo /usr/local/sbin/forgejo generate secret INTERNAL_TOKEN

   Copy `etc/forgejo/app.ini.example` -> `/etc/forgejo/app.ini`, paste the two
   secrets, `chown _forgejo:_forgejo` it, `chmod 640`.

2. Dirs: docroot (owned by deploy user so rsync can write), ACME challenge dir,
   ACME account-key dir:

       doas install -d -o chris -g daemon -m 755 /var/www/htdocs/lomdar/www
       doas install -d -o www  -g daemon -m 755 /var/www/acme
       doas install -d -m 700 /etc/acme

3. Copy `etc/httpd.conf`, `etc/relayd.conf`, `etc/acme-client.conf` to `/etc/`.

4. Issue certs (httpd must answer :80 first). Three names: www, git, and the
   bare apex (apex is only used to 301 -> www, but needs its own cert so
   `https://lomdar.com` doesn't mismatch):

       doas rcctl enable httpd
       doas rcctl start httpd
       doas acme-client -v www.lomdar.com
       doas acme-client -v git.lomdar.com
       doas acme-client -v lomdar.com

5. Start relayd + forgejo:

       relayd -n -f /etc/relayd.conf        # syntax check
       doas rcctl enable relayd forgejo
       doas rcctl start relayd forgejo

   NOTE: `rcctl reload relayd` (SIGHUP) reloads the ruleset but NOT TLS
   keypairs. After issuing/renewing a cert you must `rcctl restart relayd`.

6. Cert renewal — `doas crontab -e`, daily, restart relayd on renewal
   (restart, not reload — see note above):

       0 2 * * * for d in www.lomdar.com git.lomdar.com lomdar.com; do acme-client "$d" && rcctl restart relayd; done

7. Create the first admin. `app.ini` has `INSTALL_LOCK = true` +
   `DISABLE_REGISTRATION = true`, so there is no web wizard/registration — use
   the CLI (run as `_forgejo` so the sqlite db ownership stays correct):

       doas -u _forgejo /usr/local/sbin/forgejo admin user create \
           --admin --username chris --email chrismhoppner@gmail.com --random-password

   Log in at `https://git.lomdar.com/`, change the password under
   `/user/settings/account`.

---

## Phase 3 — DNS

Managed in this repo (opentofu/gandi). The `git` CNAME -> `public.lomdar.com.`
is in `zones/lomdar.com/records.tf`. Apply BEFORE issuing the git cert (ACME
http-01 needs the name resolving). From the repo root:

    mise exec -- tofu plan / apply
    getent hosts git.lomdar.com   # -> 89.127.234.177  (no dig in base / on dev box)

---

## Phase 4 — Blog deploy

`blog-push.sh` uses rsync over ssh, which needs rsync on BOTH ends — it is not
in OpenBSD base, so on the box once:

    doas pkg_add rsync

Build the site on the dev box, then ship it:

    system/openbsd/public/deploy/blog-push.sh /path/to/site/output

(`rsync -av --delete` into `/var/www/htdocs/lomdar/www/`.)

---

## Git over SSH (host sshd, port 22)

Forgejo's git-over-ssh goes through the **system sshd**, not its builtin server.
sshd authorizes keys via `forgejo keys`; the login user is `_forgejo` (the repo
owner), so clone URLs are `_forgejo@git.lomdar.com:owner/repo.git`.

1. `app.ini` `[server]`: `DISABLE_SSH=false`, `START_SSH_SERVER=false`,
   `SSH_CREATE_AUTHORIZED_KEYS_FILE=false`, `SSH_DOMAIN=git.lomdar.com`,
   `SSH_PORT=22` (see `app.ini.example`). `rcctl restart forgejo`.

2. Append to `/etc/ssh/sshd_config` (Match blocks go at the END of the file).
   This is scoped to `_forgejo` only — it does not weaken the global hardening,
   and `_forgejo` has no password so only forced-command keys work:

       Match User _forgejo
           AuthorizedKeysCommand /usr/local/sbin/forgejo keys -e _forgejo -u %u -t %t -k %k
           AuthorizedKeysCommandUser _forgejo

   `doas sshd -t` then `doas rcctl reload sshd`.

3. Add your ssh public key in the Forgejo UI (Settings -> SSH/GPG Keys). Test:

       git clone ssh://_forgejo@git.lomdar.com/mkaito/infra.git

   Shorter typing via `~/.ssh/config` on the dev box:

       Host git.lomdar.com
           User _forgejo

Port 22 is already open in pf; no firewall change.

---

## Verify (end to end)

- `ssh chris@public.lomdar.com` ok; `ssh root@...` refused; password auth refused
- `uname -r` = `7.9`; `pfctl -sr` shows only 22/80/443 inbound
- `pkg_info | grep forgejo`; `rcctl check httpd relayd forgejo` all `ok`
- `curl -sI http://www.lomdar.com` -> 301; `https://www.lomdar.com` -> 200 (test file)
- `curl -sI https://lomdar.com` -> 301 to www (apex cert valid, no mismatch)
- `curl -sI https://git.lomdar.com` -> forgejo; valid LE chain via
  `openssl s_client -connect git.lomdar.com:443`
- `getent hosts git.lomdar.com` resolves to the IP
