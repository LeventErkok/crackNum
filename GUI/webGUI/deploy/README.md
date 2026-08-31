# Deploying the crackNum web GUI

`server.py` is `http.server` underneath: fine for localhost and for a small
internal deployment, but not a hardened production server. These files cover the
two shapes a real deployment takes, and the handful of things that actually cost
time when you set one up.

Nothing here is required to *run* the GUI -- `./server.py` and a browser is still
the whole story locally. This is only for leaving it running on a host.

| File | Role |
|---|---|
| `cracknum-tls.service` | systemd unit, loopback-only, for use behind a TLS proxy. |
| `nginx-cracknum.conf` | nginx site: terminates TLS, redirects `:80`, proxies to the app. |

## What to install on the host

Nothing, in the usual case. The published Linux release bundle contains
statically linked `crackNum` and `z3` binaries with no shared-library
dependencies, and `server.py` is Python 3.9+ stdlib only. Unpack the bundle
somewhere -- the unit below assumes `/opt/cracknum` -- point `CRACKNUM` and
`SBV_Z3` at the two binaries, and that is the deployment. There is no build step
on the host and no reason to reach for a container.

## Two modes

**Direct, plain HTTP.** Simplest thing that works on a trusted network. The app
binds the public port itself; there is no proxy. Take `cracknum-tls.service`, and
change the `ExecStart` host and port:

    ExecStart=/usr/bin/python3 /opt/cracknum/server.py --host 0.0.0.0 --port 80

A non-root service cannot bind port 80 on its own, so also add:

    AmbientCapabilities=CAP_NET_BIND_SERVICE

That capability is the whole trick -- no proxy, no root, no setuid.

**Behind nginx, with TLS.** Use both files as they are. The app stays on
`127.0.0.1:8080` where only nginx can reach it, and the unit needs no
capabilities at all because nothing privileged is being bound by the service.

Prefer the second if the URL will be shared. Not for the padlock as such, but
because `navigator.clipboard` -- which the *Copy value* and *Copy link* buttons
want -- is defined only in a secure context. Over plain HTTP from a hostname it
is `undefined`, and the buttons appear dead. `static/app.js` carries an
`execCommand` fallback for exactly this reason, so they do work either way; TLS
just puts them back on the supported path, along with anything else
secure-context-only you might add later.

## Getting a certificate

Any CA will do. Generate the key and CSR **on the host**, so the private key
never travels:

    sudo install -d -m 0700 /etc/pki/tls/private
    sudo openssl req -new -newkey rsa:2048 -nodes \
      -keyout /etc/pki/tls/private/cracknum.key \
      -out /tmp/cracknum.csr \
      -subj "/CN=cracknum.example.com/O=Your Org/L=Your City/ST=Your State/C=US" \
      -addext "subjectAltName=DNS:cracknum.example.com"
    sudo chmod 0600 /etc/pki/tls/private/cracknum.key

Check it before submitting it anywhere:

    openssl req -text -noout -verify -in /tmp/cracknum.csr \
      | grep -A3 -e 'Subject:' -e 'Requested Extensions'

You want the subject you asked for *and* an
`X509v3 Subject Alternative Name: DNS:...` section. Browsers match on the SAN and
ignore the CN entirely, so a certificate issued without one is useless no matter
how correct the CN looks. Older `openssl` silently ignores `-addext`; if the
section is missing, use a config file with a `[v3_req]` block and `-config`.

Install the signed certificate as leaf-first, followed by any intermediates, in
the single file `ssl_certificate` points at.

## Gotchas

  * **SELinux**, *where it is enforcing*. On RHEL/Rocky/Fedora nginx cannot open
    a socket to the backend until `setsebool -P httpd_can_network_connect 1`, and
    until then every request is a 502 whose error log does not explain itself.
    Check with `getsebool httpd_can_network_connect` -- but note that command
    *exits non-zero* when SELinux is disabled, so it will abort a `set -e` script
    on exactly the hosts where the whole problem does not apply.
  * **Certificate lifetime is whatever the CA gives you**, and it may be far less
    than you assume: an internal CA that documents two years can hand back six
    months. Read `notAfter` off the issued certificate rather than trusting the
    paperwork, and set the reminder from that.
  * **HSTS.** Left commented out in the nginx config on purpose. It is sticky in
    browsers and awkward to undo; turn it on once TLS is known good, not before.
  * **Static files** are read from disk per request, so updating `static/*` needs
    only a copy and a browser hard-reload -- no service restart.
  * **Editing `ExecStart` by hand** defeats any `sed` you may have scripted
    against it. Confirm what is actually running with
    `systemctl show -p ExecStart cracknum`.
  * **Certificate expiry** is the classic way one of these dies quietly. Whatever
    lifetime yours has, put a reminder somewhere well before it.

## Hardening already in the unit

`NoNewPrivileges`, `ProtectSystem=strict`, `ProtectHome`, `PrivateTmp`,
`PrivateDevices` and `RestrictNamespaces`, running as an unprivileged `cracknum`
user against a root-owned, read-only `/opt/cracknum`. The application-level
argument is in the parent `README.md` under *Safety notes*: no shell anywhere, a
fixed table of format ids, values capped at 512 characters, and a 10-second
timeout on every crack.
