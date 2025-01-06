- [ABOUT](#about)
  - [Status update 2024-03-20](#status-update-2024-03-20)
  - [Status update 2024-03-17](#status-update-2024-03-17)
  - [What is it](#what-is-it)
  - [Current status](#current-status)
- [HOWTO](#howto)
  - [How to install](#how-to-install)
  - [How to generate peer’s key?](#how-to-generate-peers-key)
  - [How to run hbs2-peer](#how-to-run-hbs2-peer)
  - [How to configure hbs2-peer](#how-to-configure-hbs2-peer)
  - [How to create a new own repo](#how-to-create-a-new-own-repo)
  - [How to launch a peer](#how-to-launch-a-peer)
  - [How to save an encrypted file
    (TBD)](#how-to-save-an-encrypted-file-tbd)
- [FAQ](#faq)
  - [Why DVCS are not actually
    distributed](#why-dvcs-are-not-actually-distributed)
  - [Okay, if centralized services are bad, why are you
    here?](#okay-if-centralized-services-are-bad-why-are-you-here)
  - [What platforms are supported
    yet?](#what-platforms-are-supported-yet)
  - [What is a “reflog”](#what-is-a-reflog)
  - [What is the fixme?](#what-is-the-fixme)
- [Contact](#contact)
- [Download](#download)
- [Support](#support)

# ABOUT

P2P CAS / Data Replication Solution

This solution facilitates decentralized P2P git repository
synchronization with automatic peer discovery, requiring no server or
service.


## Status update 2025-01-06

The project’s alive and kicking, actively used by the team
and even supporting a few other initiatives.

The development is hosted directly on hbs2, which explains
the relatively low activity on GitHub.

The hbs2 link remains the same:

- hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP

The current focus is on optimizing hbs2-git for really large
repositories, such as OpenWrt.

It’s quite a challenging task, but definitely doable. The
key is replacing SQLite with sstable-like (LSMT) files to
ensure decent read/write performance during fetch and push
operations.

For example, OpenWrt has around 700K objects and  60K+
commits and growing.

Typically, about 100K objects need to be checked per push
operation to identify and write only the new ones.

The goal of this iteration is to make hbs2-git work smoothly
with "OpenWrt"-scale projects.

The next steps will naturally focus on "Linux-scale" and
"monorepo-scale" projects.

Hopefully, the new architecture will scale well enough.

Other changes:

 - hbs2-git dashboard will be removed from scope

 - hbs2-fixer probable will be remove as well

 - issue tracker fixme-new is moved to hbs2 codebase
   and works

 - Moving hbs2-storage to sstable/lsm instead of
   git-like storage is planned but may be will be moved
   tothe next iteration.

The planned release date for the iteration is Feb'2025.

Stay tuned!


## Status update 2024-03-20

hbs2-git 0.24.1 is in master. Status =\> beta. Old hbs2-git is
discontinued. Use the new one.

Data structures are incompatible between the old and the new versions,
however, migrations is safe and all references remains the same (merely
the type of the references are changed).

## Status update 2024-03-17

We have been using hbs2 and hbs2-git for approximately 13 months.

New version hbs2-git-0.24.1 is in TEST status. A lot of changes. Big
repository support, new repository structure, new tools, simplier
workflow. Release is scheduled to 2024-W12 (week 12).

Web publishing tools are almost ready and being tested as well.

As soon as they will be ready, web site hbs2.net is about to appear.

Right now TEST branch is lwwrepo. Tag: 0.24.1-rc1

Repository is available on:

- HBS2 hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP
- HTTPS
  https://git.hbs2.net/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP
- GitHub https://github.com/voidlizard/hbs2.git

## What is it

It is an experimental distributed P2P content addressable storage with
content distribution protocols and tools.

It may be used for storing and distributed syncronization of data.

HBS2 is aimed to take care of:

- NAT traversing
- Peer discovery
- Notification
- Distribution
- Encryption
- Validation (hashes checking, signatures checking)
- Storing and obtaining data

In short, you store data in this storage, and all subscribers are
notified of it and receive a copy of the data.

It is a middleware for implementing distributed applications that shares
data. Like a distributed git, for example. (What? git is already
distributed and… No, it is not. Not really).

The idea of extracting the minimal sufficent set of primitives for
distributed applications and APIs and let the side applications do the
rest.

This is not a “blockchain”, but heavily uses the approaches that
“blockchains” brought to the world.

Using this solution you may treat application data as local. HBS2 will
syncronize all the data along the crowd of peers. The apps don’t need to
bother where the other peers are located, where the hosts, ssh keys on
thouse hosts, auth tokens on thouse hosts, etc. They only need to know
the references and (optionally) have signing/encryption keys that are
stored locally or distributed (public parts, of course) automatically
like any other data.

What types of applications may be implemented on top of this?

For an instance:

- Distributed file sharing (wip)
- Distributed git (seems working)
- Distributed communications, like a chat or a “channel”
- Distibuted ledgers with different types of consensus protocols (we’re
  trying not to use “b” words)
- Actually, any sort of applications that require data and network

The whitepaper is in shortlist, watch the updates.

Why it is *experimental* ? Well, it’s on a quite early stage and some
root data structures, protocols or API may change.

It also have some known issues with performance and might have some
stability issues. We’re working hard to fix them.

## Current status

Version 0.24.1-rc.

Means it’s mostly working. We’re using it about a year.

Encryption status: works.

Encryption for arbitrary merkle trees/blocks: implemented, works, being
tested.

Encryption for protocols: implemented, turned on:

So right now it is useful for distributing any data.

We’re using it for our non-public projects.

# HOWTO

## How to install

### nix flakes

Assuming you know what the Nix and Nix flakes are ( See
[nixos.org](https://nixos.org) if you don’t )

and nix flake support is turned on on your system:

    nix profile install github:voidlizard/hbs2/master

It will take time. Patience, we’re working on rolling out cachix, that
will allow binary caches for the project.

Alternative option:

    nix profile install git+http://git.hbs2.net/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP \
    --substituters http://nix.hbs2.net:6000 \
    --trusted-public-keys git.hbs2.net-1:HYIYU3xWetj0NasmHrxsWQTVzQUjawOE8ejZAW2xUS4=

### Home Manager module

The following snippet of code tries to show how to bring the HBS2 flake
from the flake input and use its packages with Home Manager.

Don’t forget to replace exampleName with your username!

```nix
# flake.nix

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hbs2.url = "git+https://git.hbs2.net/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP";
  };

  outputs = {nixpkgs, home-manager, hbs2, ...}: {
    homeConfigurations."exampleName" =
    let system = "x86_64-linux"; # use your system here
    in home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};

      modules = [
        hbs2.homeManagerModules.${system}.default
        {
          services.hbs2 = {
            enable = true;
            git-dashboard.enable = true; # optional
          };
        }
        # ...
      ];
    };
  };
}
```

Option `services.hbs2.enable` will add all hbs2 binaries into your environment and
create `hbs2-peer` user service to run it automatically at the background.

Option `services.hbs2.git-dashboard.enable` will create `hbs2-git-dashboard` user service.

## How to generate peer’s key?

    hbs2 keyring-new > new-peer-key.key

## How to run hbs2-peer

hbs2-peer run \[-c config\]

config is a path to a **directory** with hbs2-peer config.

By default it is \$HOME/.config/hbs-peer

## How to configure hbs2-peer

There are quite a lot of options even for today and we denitely need
staring work on a manual. But here is a minimal working example:

Typically hbs2-peer config is located at

\$HOME/.config/hbs2-peer/config

    ; ip/port to for UDP
    listen "0.0.0.0:7351"

    ; tcp
    listen-tcp "0.0.0.0:10351"

    ; port for HTTP service.
    ; it's on you to pass it outside or not.
    ; optional

    http-port 5001

    ; path to the peer's key
    ; used to identify peers

    key    "./key"

    ; path to storage. optional
    ; storage  "/root/.local/share/hbs2"

    ; may be omitted, default location
    ; will be used then

    accept-block-announce *

    ; accept blocks from everyone
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


    ; address for dns bootstrapping
    bootstrap-dns "bootstrap.hbs2.net"

    ; just and example. it's my test container
    ; known-peer "10.250.0.1:7354"
    ; known-peer "10.250.0.1:7351"
    ; you may add own peers like this
    ; or use your own domains for dns bootstrapping

    ; poll certain reference
    poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"

## How to create a new own repo

1.  Create a new keyring

<!-- -->

    hbs2 keyring-new > new.key

2.  Watch it’s public key

<!-- -->

    hbs2 keyring-list new.key

Example:

    [user@host:~/dir]$ hbs2 keyring-list ./new.key
    sign-key:  eq5ZFnB9HQTMTeYasYC3pSZLedcP7Zp2eDkJNdehVVk

3.  Export repo to the new reflog

<!-- -->

    git hbs2 export --public --new eq5ZFnB9HQTMTeYasYC3pSZLedcP7Zp2eDkJNdehVVk

4.  Add git remote and push

<!-- -->

    git remote add mynerepo hbs2://eq5ZFnB9HQTMTeYasYC3pSZLedcP7Zp2eDkJNdehVVk
    git push mynerepo

5.  Wait some time

6.  Work with git as usual

## How to launch a peer

Example:

    hbs2-peer run

## How to save an encrypted file (TBD)

    keyring-new > kr
    keyring-list kr
    ; create a file with a list of public keys
    ; copy the lines from the output of the keyring-list command
    groupkey-new path/to/file/with/list/of/pubkeys > groupkey
    store --groupkey groupkey file/to/store
    ; get the hash
    cat --keyring kr <hash>

# FAQ

## Why DVCS are not actually distributed

Reason 1. Because they don’t have any content distribution mechanism.

Common practice right now is using centralized services, which are:

- Censored
- Faulty
- Not transparent and irresponsible (For customers. They are responsible
  as hell for any sort of goverment-alike structures before they even
  asked for something).
- Tracking users
- May use their code regardless of license agreement
- Giving up the network neutrality in a sake of \<skipped\*\> anyone but
  customers who pay

There are registered examples, how one most popular git service droppped
repositoties because they contain some words in README file.

And banned accounts for visiting the service from wrong IP address.

And data loss in a cloud storage services because they located all
replicas in a single data centre which was destroyed by the fire or a
canalization breakthrough. They even don’t tell you how many replicas do
they have for your data. Why? Because fuck you, that’s why.

Setting own hosts/services for dvcs data hosting.

Yeah, it’s the way. But they are

- Obviously centralized

and also:

- Domain name system is compromised
- Certificate system is compromised by so many ways.

Why? Because they are ruled by commercial companies working in certaing
jurisdictions.

What else. Sending patches by email.

- Looks more like anecdote today (but still used by someone)
- Email right now is a centralized service with all the consequences
  (see above)

Okay, ley’s bring the overlay network (VPN), place all our hosts and
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

Things like Syncthing don’t scales, in fact event if you will use git
repo in syncthing dir, you will face file modification conflicts even if
you use them alone.

So that’s why HBS2 came to light. Trust me, if I could use some
decentralized solution normally for this I’d never start this project.

## Okay, if centralized services are bad, why are you here?

Is’s a mirror for the really distributed repository:

hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP

## What platforms are supported yet?

So far we were able to run the hbs2-peer on:

- NixOS ( x86_64-linux )
- Windows WSL+Ubuntu
- Debian/rasberri-pi (aarch64-linux)

Probably it will work on MacOS - but we need someone to check.

## What is a “reflog”

Reflog is an implementation of a permanent mutable reference. It has a
permanent ID that corresponds to a public signing cryptographic key, and
the value, that is calculated from the “state”, where the state is a set
of all “reference update” transactions.

Each transaction is cryptographically signed by the sender, for current
reflog implementation sender must be an owner of the private key of the
public key.

For this type of references, only transactions that are properly signed
by the mentioned private key are accepted at the moment.

Therefore, reflog is a log of signed transactions. Content of thouse
transaction is up to an application.

For the hbs2-git it is an reference to a merkle tree, that contains the
state of repository ( branches + all objects accessible from thouse
branches ).

So, reflog is a sort of reference which state is defined by the set of
signed binary transactions. The payload of the transactions mauy be
arbitrary and application-dependent, but they must be properly signed by
the owner of the private key.

As there is only one valid writer for this type of reference, all
transactions are assigned a Sequential Number that establishes their
order. Applications may use this order to determine the sequence of
transactions.

Should be all reflogs on all hosts have the same value?

Well. It would be nice, but not nesessary. But eventually yes, they
will. If there is really only one writer and it is not writing all the
time.

## What is the fixme?

[fixme](https://github.com/voidlizard/fixme)

# Contact

telegram: @voidlizard

# Download

hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP

Note! This is not a bitcoin address. If you want a bitcoin address to
donate, use the other one (TBD).

# Support

Contribute! Code or ideas or share the experience or any suggestions.
