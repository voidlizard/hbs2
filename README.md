-   [ABOUT](#about)
    -   [What is it](#what-is-it)
-   [HOWTO](#howto)
    -   [How to run hbs2-peer](#how-to-run-hbs2-peer)
    -   [How to make a pull request](#how-to-make-a-pull-request)
    -   [How to launch a peer](#how-to-launch-a-peer)
    -   [How to save an encrypted file
        (TBD)](#how-to-save-an-encrypted-file-tbd)
-   [FAQ](#faq)
    -   [Why DVCS are not actually
        distributed](#why-dvcs-are-not-actually-distributed)
    -   [Okay, if centralized services are bad, why are you
        here?](#okay-if-centralized-services-are-bad-why-are-you-here)

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

# HOWTO

## How to run hbs2-peer

hbs2-peer run \[-c config\]

config is a path to a **directory** with hbs2-peer config.

By default it is \$HOME/.config/hbs-peer

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

Each update is subscribed with the private key from the keypair, so only
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

Reason 1. Because they don have a content distribution mechanisms

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

Setting own hosts/services for dvcs data hosting.

Yeah, it\'s the way. But they are

-   Obviously centralized

and also:

-   Domain name system is compomised
-   Certificate system is compomised by so many ways.

Why? Because they are ruled by commercial companies working in certaing
jurisdictions.

What else. Sending patches by email.

-   Looks more like anecdote today (but still used be someone)
-   Email right now is a centralized service with all the consequences
    (see above)

Okay, ley\'s bring the overlay network (VPN), place all our hosts and
resources there and will use own DNS.

Yeap, it will work. But is it will cost you. It is acceptable for an
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

Contact:

telegram: @voidlizard


