залил в мастер базовый hbs2-reposync. что он делает:  берёт пачку hbs2-git репо и синхронизирует их
с git bare repo, которые где-то лежат.

зачем: что бы третьи программы (в первую очередь nix), которые не могут использовать
git-remote-helper - могли выкачивать репо и мы могли бы ссылаться в зависимостях на свои репозитории
из hbs2-git.

пример использования:

```
[dmz@minipig:~/w/hbs2]$ cat hbs2-git/reposync/examples/config

rpc unix "/tmp/hbs2-rpc.socket"

; http-port 4017

; root "/home/dmz/.local/share/hbs2-reposync/repo"

;; single reflog

[ reflog "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
;; options may go here if any
]

[ reflog  "JCVvyFfj1C21QfFkcjrFN6CoarykfAf6jLFpCNNKjP7E"
  (decrypt "/home/dmz/w/hbs2/owner.key")
]

[dmz@minipig:~/w/hbs2]$ ./bin/hbs2-reposync -c hbs2-git/reposync/examples/config  run
```

стартует и слушает на порту 4017 (настраивается в конфиге).  далее мы неведомым способом
перенаправляем порт 80 на него, и делаем так, что бы какой-то хост указывал на ip,  на котором
слушает hbs2-reposync. например, у меня так:

```
[dmz@minipig:~/w/hbs2]$ cat /etc/hosts

127.0.0.1  git.hbs2

cat /nix/store/47wrg18zgwb6sy5p0k1ahapv9i7fiz2n-Caddyfile-formatted/Caddyfile

http://git.hbs2 {
  bind

  log {
    output file /var/log/caddy/access-http://git.hbs2.log
  }

  reverse_proxy http://git.hbs2:4017
}

```

теперь будет работать такое:

```
[dmz@minipig:~/tmp]$ git clone http://git.hbs2/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP wtf1
Cloning into 'wtf1'...
```

и можно во флейках ссылаться на

```
git+http://git.hbs2/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP
```

поддержка шифрования репозиториев в каком-то виде: есть, у меня получилось.
но при копировании в git bare репозиторий всё будет расшифровано.
если важна секретность, надо держать его где-то на шифрованном разделе
или типа того.




