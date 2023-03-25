
## How to run hbs2-peer

hbs2-peer run [-c config]

config is a path to a **directory** with hbs2-peer config.

By default it is $HOME/.config/hbs-peer


## How make a pull request

Since the goal of this project is to move away from
centralized services, pull requests should be done by
decentralized fashion.

It may seem like there are too many steps here below, but
it's a full setup for creating a new distributed repo,
subscribing to changes and distributing your own changes. So
its a complete setup from the scratch to use distributed
repos with hbs2.  You don't need them all if you have
already set it up once.

In short:

1. Setup the hbs2
2. Make it listen the reflog for the repo
2. Clone the repo from hbs2
3. Create a new keypair
4. Create a new reflog with the keypair
5. Export the repo to a new reflog
6. Add the repo as a new git remote
7. Work with git as usuall, push to the new created repo

Each update is subscribed with the private key from the
keypair, so only the person who has the private key may
update the reflog. In fact, public key IS the reflog,
and the private key is a proof of ownership.

Full procedure:

1. Download, install and run hbs2 project. On this stage
   of the project it is supposed that you are able to
   install the project using the flake.nix.

   Right now, it will take a time, so be ready to it.


2. Optional*. Make hbs2-peer poll this topic:

```
echo poll reflog 1 "2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX" >> <your-hbs2-peer-config>
```

<your-hbs2-peer-config> is typically
~/.config/hbs2-peer/config  but it may vary up to setup

3. Fetch the *reflog* (topic) for the repo:

```
hbs2-peer reflog fetch 2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX
```

If you have the set up as in step 2, it will be done
periodically and upon hbs2-peer start, so you don't have
to bother.  Also, hbs2-peer after step 2 will listen the
reflog, so new pushes will be delivered automatically.

4. Check the reflog is here:

```
hbs2-peer reflog get 2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX
```

Note, that it may take time to all objects to deliver.

5. Clone the project

```
git clone hbs2://2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX hbs2
```

6. Create your own topic

```
hbs2 keyring-new > my-keyring.key
hbs2 keyring-list my-keyring.key

[user@host:~]$ hbs2 keyring-list my-keyring.key
sign-key:  6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
this is your new repo's relog (topic)
Of course, sign-key will be different.

```
Keep the keyring file private. It contains the key pair
(private+public keys) that wilyl allow you to write to the
reflog. If you lose it, you will lose the write access
to your repo. It's not a big deal, creating keypairs is
cheap. But you will need to tell anyone update theirs
references to a new repo.

7. Export the repo to the new reflog (topic).

```
git hbs2 export <sign-key> -k <keyring-file>
```

In this example, sign-key will be 6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d
<keyring-file> will be my-keyring.key

This step will export all objects to the new created
topic.

It will take time. hbs2-git will copy all objects from
the repository to a new reflog. Although it wan't create
objects that are already in the hbs2, it takes time to
calculate hashes and check it out. So be prepared to wait
quite a while, but only for the first time.

8. Locate the configuration file and add the keyring

Example:

```
[user@host:~]$ cat ~/.config/hbs2-git/w/hbs2/config

branch "master"

keyring "/home/user/secrets-dir/my-keyring.key"

```

Note, that keyring file must be absolute. And
the location supposed to be safe.

In my case, it's a mounted encrypted directory, but it's
up to you.

9. Add the new repo to a git

```
git remote add mytopic hbs2://<sign-key>
```

Example:

```
git remote add mytopic hbs2://6CMRnptW8DjiW4S1kv3U6wEAUGwhZmG7522fsqi3SH2d

```

```
git fetch origin
git fetch mytopic
```

You may want to set your own topic as the "origin," and
another topic as something else. It's completely up to you.
It works just like setting up Git remotes in the
usual way.


10. Make your changes

11. Commit

12. Describe your changes somewhere, using PR: prefix

See .fixme/config file to get an idea what files are
scanned for issues/pull requests/etc.

PR is a just a fixme entry (look for fixme description)
which describes the pull requests. It just a text with
textual references to a branch, commit and other
information required for merging the changes.


Example:

docs/.../some-pr-file

```
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

Now, if the author of the original topic (reflog, repo) is
aware abour your topic and subscribed to if (added it as a
remote to his/her git repo) they will be able to receive
and merge your pull requests.

15. Check the reflog (just for in case)

```
hbs2-peer reflog get <sign-key>
```

## How to launch a peer

Example:
```

hbs2-peer run -p .peers/1  -k .peers/1/key -l addr:port -r rpcaddr:rpcport

```


## Как сохранять зашифрованный файл

```
keyring-new > kr
keyring-list kr
; создаём файл со списком публичных ключей
; строчки из выхлопа команды keyring-list
groupkey-new path/to/file/with/list/of/pubkeys > groupkey
store --groupkey groupkey file/to/store
; получаем хэш
cat --keyring kr <хэш>
```
