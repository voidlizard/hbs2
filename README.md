-   [ABOUT](#about)
    -   [What is it](#what-is-it)
-   [HOWTO](#howto)
    -   [How to install](#how-to-install)
    -   [How to run hbs2-peer](#how-to-run-hbs2-peer)
    -   [How to configure hbs2-peer](#how-to-configure-hbs2-peer)
    -   [How to make a pull request](#how-to-make-a-pull-request)
    -   [How to launch a peer](#how-to-launch-a-peer)
    -   [How to save an encrypted file
        (TBD)](#how-to-save-an-encrypted-file-tbd)
    -   [How to save an encrypted file
        (TBD)](#how-to-save-an-encrypted-file-tbd)
-   [FAQ](#faq)
    -   [Why DVCS are not actually
        distributed](#why-dvcs-are-not-actually-distributed)
    -   [Okay, if centralized services are bad, why are you
        here?](#okay-if-centralized-services-are-bad-why-are-you-here)
    -   [What platforms are supported
        yet?](#what-platforms-are-supported-yet)
    -   [What is a \"reflog\"](#what-is-a-reflog)
    -   [What is the fixme?](#what-is-the-fixme)
-   [Contact](#contact)
-   [Support](#support)
-   [Donate](#donate)

# ABOUT

## What is it

It is an experimental distributed P2P content addressable storage with
content distribution protocols.

It may be used for storing and distributed syncronization of data.

HBS2 is aimed to take care of:

-   NAT traversing
-   Peer discovery
-   Notification
-   Distribution
-   Encryption
-   Validation (hashes checking, signatures checking)
-   Storing and obtaining data

In short, you store data in this storage, and all subscribers are
notified of it and receive a copy of the data.

It is a middleware for implementing distributed applications that shares
data. Like a distributed git, for example. (What? git is already
distributed and\... No, it is not. Not really).

The idea of extracting the minimal sufficent set of primitives for
distributed applications and APIs and let the side applications do the
rest.

This is not a \"blockchain\", but heavily uses the approaches that
\"blockchains\" brought to the world.

Using this solution you may treat application data as local. HBS2 will
syncronize all the data along the crowd of peers. The apps don\'t need
to bother where the other peers are located, where the hosts, ssh keys
on thouse hosts, auth tokens on thouse hosts, etc. They only need to
know the references and (optionally) have signing/encryption keys that
are stored locally or distributed (public parts, of course)
automatically like any other data.

What types of applications may be implemented on top of this?

For an instance:

-   Distributed file sharing (wip)
-   Distributed git (seems working)
-   Distributed communications, like a chat or a \"channel\"
-   Distibuted ledgers with different types of consensus protocols
    (we\'re trying not to use \"b\" words)
-   Actually, any sort of applications that require data and network

The whitepaper is in shortlist, watch the updates.

Why it is *experimental* ? Well, it\'s on a quite early stage and some
root data structures, protocols or API may change.

It also have some known issues with performance and might have some
stability issues. We\'re working hard to fix them.

## Current status

MVP. Means that it is not fully working, but it's
useful. It is useful right now for data sharing,
including git repositories.

Encryption status: wip, partially works.

Encryption for arbitrary merkle trees/blocks:
implemented, not really tested.

Encryption for protocols: wip, right now is turned
off!

So right now it is useful for distributing any non-sensitive
non-private data, like opensource projects, which is exactly
what this project is.


# HOWTO

## How to install

Assuming you know what the Nix and Nix flakes are
( See [nixos.org](https://nixos.org) if you don't )

and nix flake support is turned on on your system:

```
nix profile install github:voidlizard/hbs2/master
```

It will take time. Patience, we're working on rolling
out cachix, that will allow binary caches for the
project.

## How to generate peer's key?

```
hbs2 keyring-new > new-peer-key.key
```

## How to run hbs2-peer

hbs2-peer run \[-c config\]

config is a path to a **directory** with hbs2-peer config.

By default it is \$HOME/.config/hbs-peer

## How to configure hbs2-peer

There are quite a lot of options even for today and we
denitely need staring work on a manual. But here is a
minimal working example:

Typically hbs2-peer config is located at

$HOME/.local/hbs2-peer/config

```

; ip/port to for UDP
listen "0.0.0.0:7351"

; ip/port for rpc
rpc    "127.0.0.1:13331"

; port for HTTP service.
; required by hbs2-git
; it's on you to pass it outside or not.

http-port 5001

; path to the peer's key
; used to identify peers

key    "./key"

; path to storage. optional
; storage  "/root/.local/share/hbs2"

; may be omitted, default location
; will be used then

accept-block-announce *

; accept blocks from anyone
; by default is disabled

; you may allow only a few peers
; to send announces like

; accept-block-announce "peer-public-key"
; peer-public-key may be obtained from keyring file:
; hbs2 keyring-list ./key
; [user@host:~/hbs2]# hbs2 keyring-list /etc/hbs2-peer/key
;
; sign-key:  4543L9D1rr8M8Zzgxc76fRGjUyWF8rdsmiUMfCwF1RnA
;
; it's a public information.
; but keep peer key file in private place!


; download-log "/tmp/download-log"
; where to place a "dowload-log"
; may be omitted, default location will be used then

; address for dns bootstrapping
bootstrap-dns "bootstrap.hbs2.net"

; just and example. it's my test container
; known-peer "10.250.0.1:7354"
; known-peer "10.250.0.1:7351"
; you may add own peers like this
; or use your own domains for dns bootstrapping

; poll certain reference
poll reflog 1 "2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX"

; means poll reflog "2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX"
; once per minute


```


## How to make a pull request

Since the goal of this project is to move away from centralized
services, pull requests should be done by decentralized fashion.

It may seem like there are too many steps here below, but it\'s a full
setup for creating a new distributed repo, subscribing to changes and
distributing your own changes. So its a complete setup from the scratch
to use distributed repos with hbs2. You don\'t need them all if you have
already set it up once.

In short:

1.  Setup the hbs2
2.  Make it listen the reflog for the repo
3.  Clone the repo from hbs2
4.  Create a new keypair
5.  Create a new reflog with the keypair
6.  Export the repo to a new reflog
7.  Add the repo as a new git remote
8.  Work with git as usuall, push to the new created repo

Each update is signed with the private key from the keypair, so only
the person who has the private key may update the reflog. In fact,
public key IS the reflog, and the private key is a proof of ownership.

Full procedure:

1.  Download, install and run hbs2 project. On this stage of the project
    it is supposed that you are able to install the project using the
    flake.nix.

    Right now, it will take a time, so be ready to it.

2.  Optional\*. Make hbs2-peer poll this topic:

```
echo poll reflog 1 "2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX" >> <your-hbs2-peer-config>
```

`<your-hbs2-peer-config>` is typically
\~/.config/hbs2-peer/config but it may vary up to setup

3.  Fetch the *reflog* (topic) for the repo:

```
hbs2-peer reflog fetch 2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX
```

If you have the set up as in step 2, it will be done periodically and
upon hbs2-peer start, so you don\'t have to bother. Also, hbs2-peer
after step 2 will listen the reflog, so new pushes will be delivered
automatically.

4.  Check the reflog is here:

```
hbs2-peer reflog get 2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX
```

Note, that it may take time to all objects to deliver.

5.  Clone the project

```
git clone hbs2://2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX hbs2
```

6.  Create your own topic

```
hbs2 keyring-new > my-keyring.key
hbs2 keyring-list my-keyring.key

[user@host:~]$ hbs2 keyring-list my-keyring.key
sign-key:  6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
this is your new repo's relog (topic)
Of course, sign-key will be different.
```

Keep the keyring file private. It contains the key pair (private+public
keys) that wilyl allow you to write to the reflog. If you lose it, you
will lose the write access to your repo. It\'s not a big deal, creating
keypairs is cheap. But you will need to tell anyone update theirs
references to a new repo.

7.  Export the repo to the new reflog (topic).

```
git hbs2 export <sign-key> -k <keyring-file>
```

In this example, sign-key will be
6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d `<keyring-file>`
will be my-keyring.key

This step will export all objects to the new created topic.

It will take time. hbs2-git will copy all objects from the repository to
a new reflog. Although it wan\'t create objects that are already in the
hbs2, it takes time to calculate hashes and check it out. So be prepared
to wait quite a while, but only for the first time.

8.  Locate the configuration file and add the keyring

Example:

```
[user@host:~]$ cat ~/.config/hbs2-git/w/hbs2/config

branch "master"

keyring "/home/user/secrets-dir/my-keyring.key"
```

Note, that keyring file must be absolute. And the location supposed to
be safe.

In my case, it\'s a mounted encrypted directory, but it\'s up to you.

9.  Add the new repo to a git

```
git remote add mytopic hbs2://<sign-key>
```

Example:

```
git remote add mytopic hbs2://6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d

git fetch origin
git fetch mytopic
```

You may want to set your own topic as the \"origin,\" and another topic
as something else. It\'s completely up to you. It works just like
setting up Git remotes in the usual way.

10. Make your changes

11. Commit

12. Describe your changes somewhere, using PR: prefix

See .fixme/config file to get an idea what files are scanned for
issues/pull requests/etc.

PR is a just a fixme entry (look for fixme description) which describes
the pull requests. It just a text with textual references to a branch,
commit and other information required for merging the changes.

Example:

```
docs/\.../some-pr-file

PR: my-very-first-pr
  Just to test the concept. It may be merged from the
  branch

  branch: pr-XXX-my-very-first
  commit: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

```

13. Commit

14. Push

```
git push mytopic
```

Now, if the author of the original topic (reflog, repo) is aware abour
your topic and subscribed to if (added it as a remote to his/her git
repo) they will be able to receive and merge your pull requests.

15. Check the reflog (just for in case)


```
hbs2-peer reflog get <sign-key>
```

## How to launch a peer

Example:

```
hbs2-peer run -p .peers/1  -k .peers/1/key -l addr:port -r rpcaddr:rpcport
```

## How to save an encrypted file (TBD)

```
keyring-new > kr
keyring-list kr
; create a file with a list of public keys
; copy the lines from the output of the keyring-list command
groupkey-new path/to/file/with/list/of/pubkeys > groupkey
store --groupkey groupkey file/to/store
; get the hash
cat --keyring kr <hash>
```

# FAQ

## Why DVCS are not actually distributed

Reason 1. Because they don't have any content distribution mechanism.

Common practice right now is using centralized services, which are:

-   Censored
-   Faulty
-   Not transparent and irresponsible (For customers. They are
    responsible as hell for any sort of goverment-alike structures
    before they even asked for something).
-   Tracking users
-   May use their code regardless of license agreement
-   Giving up the network neutrality in a sake of \<skipped\*\> anyone
    but customers who pay

There are registered examples, how one most popular git service droppped
repositoties because they contain some words in README file.

And banned accounts for visiting the service from wrong IP address.

And data loss in a cloud storage services because they
located all replicas in a single data centre which was
destroyed by the fire or a canalization breakthrough.
They even don't tell you how many replicas do they have
for your data. Why? Because fuck you, that's why.


Setting own hosts/services for dvcs data hosting.

Yeah, it\'s the way. But they are

-   Obviously centralized

and also:

-   Domain name system is compromised
-   Certificate system is compromised by so many ways.

Why? Because they are ruled by commercial companies working in certaing
jurisdictions.

What else. Sending patches by email.

-   Looks more like anecdote today (but still used by someone)
-   Email right now is a centralized service with all the consequences
    (see above)

Okay, ley\'s bring the overlay network (VPN), place all our hosts and
resources there and will use own DNS.

Yeap, it will work. But it will cost you. It is acceptable for an
organisation, but hardly for a group of random people.

What else.

Imagine, you generate a couple of cryptographic keys, drop the repo to a
folder and it distributes by torrents as easy as any other torrents.
Fully encrypted and only certain subscribers could decrypt and use the
data.

Well, torrent are brilliant, but they not just not designed to do things
like this easily.

Also they require trackers, that are centralized web resources.

Things like Syncthing don\'t scales, in fact event if you will use git
repo in syncthing dir, you will face file modification conflicts even if
you use them alone.

So that\'s why HBS2 came to light. Trust me, if I could use some
decentralized solution normally for this I\'d never start this project.

## Okay, if centralized services are bad, why are you here?

I\'m here yet. hbs2-git works only a few days, but it works now. So
since 2023-03-23 this service is not an only one.

And since 2023-03-24 I consider the

hbs2://2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX

as the main project repository.

## What platforms are supported yet?

So far we were able to run the hbs2-peer on:

 - NixOS ( x86_64-linux )
 - Windows WSL+Ubuntu
 - Debian/rasberri-pi (aarch64-linux)

Probably it will work on MacOS - but we need
someone to check.

## What is a "reflog"

Reflog is an implementation of a permanent mutable
reference. It has a permanent ID that corresponds to a
public signing cryptographic key, and the value, that
is  calculated from the "state", where the state  is
a set of all "reference update" transactions.

Each transaction is cryptographically signed by the sender,
for current reflog implementation sender must be an owner of
the private key of the public key.

For this type of references, only transactions that are
properly signed by the mentioned private key are accepted at
the moment.

Therefore, reflog is a log of signed transactions. Content
of thouse transaction is up to an application.

For the hbs2-git it is an reference to a merkle tree, that
contains the state of repository ( branches + all objects
accessible from thouse branches ).

So, reflog is a sort of reference which state is defined by
the set of signed binary transactions. The payload of the
transactions mauy be arbitrary and application-dependent,
but they must be properly signed by the owner of the private
key.

As there is only one valid writer for this type of
reference, all transactions are assigned a Sequential Number
that establishes their order. Applications may use this
order to determine the sequence of transactions.

Should be all reflogs on all hosts have the same value?

Well. It would be nice, but not nesessary. But eventually
yes, they will. If there is really only one writer and it is
not writing all the time.


## What is the fixme?

[fixme](https://github.com/voidlizard/fixme)


# Contact

telegram: @voidlizard

# Download

hbs2://2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX

Note! This is not a bitcoin address. If you want a bitcoin
address to donate, use the other one (TBD).

# Support

Contribute! Code or ideas or share the experience or
any suggestions.


# Donate

TBD.

# Other

Just a random string to measure github push time vs hbs2.

```
[dmz@minipig:~/w/hbs2]$ time git push github
Enumerating objects: 5, done.
Counting objects: 100% (5/5), done.
Delta compression using up to 16 threads
Compressing objects: 100% (3/3), done.
Writing objects: 100% (3/3), 360 bytes | 360.00 KiB/s, done.
Total 3 (delta 2), reused 0 (delta 0), pack-reused 0
remote: Resolving deltas: 100% (2/2), completed with 2 local objects.
To github.com:voidlizard/hbs2.git
   4b6af37..89de740  master -> master

real	0m1,753s
user	0m0,012s
sys	0m0,011s

[dmz@minipig:~/w/hbs2]$ time git push hbs2
importing objects [=========================] 100%
calculate dependencies
storing dependencies [======================] 100%
store objects [=============================] 100%
To hbs2://2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX
   2f8e928..89de740  master -> master

real	0m1,354s
user	0m0,663s
sys	0m0,198s
```

