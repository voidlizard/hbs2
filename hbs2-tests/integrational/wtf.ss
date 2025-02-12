
(define local-net macvlan-wtf)
(define remote-net macvlan1)
(define local-phy enp2s0)

(define local-ip   :192.168.1.171/24)
(define remote-ip  :192.168.1.172/24)
(define remote-gw  :192.168.1.1)

(define local-temp-dir (sys:temp:dir))
(define local-conf     (join :/ local-temp-dir .config))
(define local-share    (join :/ local-temp-dir .share))
(define local-local    (join :/ local-temp-dir .local))
(define local-keyman   (join :/ local-temp-dir .hbs2-keyman/keys))
(define local-temp     (join :/ local-temp-dir .tmp))
(define resolv.conf    (join :/ local-temp-dir :resolv.conf))
(define workdir        (pwd))
(define locale-dir /run/current-system/sw/lib/locale/locale-archive)

(define HOME (env :HOME))

(define profile-bin (join :/ /home/dmz/.nix-profile/bin))

(define local-dirs
  `[ ,local-temp-dir
     ,local-conf
     ,local-share
     ,local-local
     ,local-keyman
     ,local-temp
   ])

(define remote-path
  (begin
    (local path0
      `[ /usr/bin:/sbin
         /usr/sbin
         /nix/store
         /opt/bin
         /run/current-system/sw/bin
         ,(sym HOME /.nix-profile/bin)
         /opt/bin
         ,(join :/ workdir bin)
       ])
    (join :: path0)))

(define tmux.conf
  (begin
    (local tenv (join space set-environment -g PATH (str remote-path)))
    (local cfg (join chr:lf tenv chr:lf) )
   cfg)
)

(define env-file (sym (join :/ local-temp .env)))

(define vm-shell
  (begin
    (local vminit
       `[ [call:proc ip link set ,remote-net up]
          [call:proc ip addr add ,remote-ip dev ,remote-net]
          [call:proc ip route add default via   ,remote-gw]

          ;; fucked up network
          [call:proc tc qdisc add dev ,remote-net root netem delay 250ms 40ms loss 1%]

          ; [str:append:file /etc/resolv.conf (concat nameserver space ,remote-gw chr:lf)]
          [run:proc:attached echo ,HOME]
          [println "PATH: "  (env PATH)]
          [cp /tmp/.tmux.conf (join :/ (env HOME) .tmux.conf)]
          ; [run:proc:attached bash --noprofile --norc]
          [run:proc:attached tmux new-session -d -s mysession sh]
          [run:proc:attached tmux split-window -v -t mysession:0 sh]
          [run:proc:attached tmux send-keys -t mysession:0.0 hbs2-keyman space update Enter]
          [run:proc:attached tmux send-keys -t mysession:0.0 hbs2-peer space run Enter]
          [sleep 0.5]
          [run:proc:attached tmux send-keys -t mysession:0.1 "hbs2-peer log debug on" Enter]

          [run:proc:attached tmux send-keys -t mysession:0.1 "hbs2-peer ping 192.168.1.43:7354" Enter]
          [sleep 0.5]

          [run:proc:attached tmux send-keys -t mysession:0.1 "git clone hbs23://EvP3kskPVuKuKVMUc3LnfdW7GcFYjz6f5fFU1EGzrdgk && tmux kill-session -t mysession" Enter]
          [run:proc:attached tmux attach-session -t mysession ]

          ; [run:proc:attached tmux attach-session -t mysession]
          ; [run:proc:attached tmux kill-session -t mysession]
        ])
    vminit
  )
)

(define (vm-shell-write)
  (begin
    (touch env-file)
    (str:save env-file (sym (join chr:lf vm-shell)))
    (str:save resolv.conf (sym :nameserver space remote-gw))
    (str:save (join :/ local-temp .tmux.conf) tmux.conf)
  )
)

(define (nspawn-args)
  (begin '[]
    (local ndir  (join := --directory :/))
    (local iface (join := --network-macvlan (concat local-net :: remote-net)))
    (local (param k v) (join := k v))
    (local (nbind a b)   (join := --bind (join :: a b)))
    (local (nbind-ro a b)   (join := --bind-ro (join :: a b)))
    (local local-locale-dir  /run/current-system/sw/lib/locale/locale-archive)
    (local remote-locale-dir /usr/lib/locale/locale-archive)
    (local dist-dir (join :/ workdir dist-newstyle))
    (local remote-keyman /root/.hbs2-keyman/keys)
    (local setenv --setenv)
    (local nargs0
           `[ ,ndir
              --ephemeral
              ,(nbind local-keyman remote-keyman)
              ,(nbind local-temp /tmp)
              ,(nbind /var/tmp /var/tmp)
              ,(nbind /run/current-system/sw/bin /run/current-system/sw/bin)
              ,(nbind resolv.conf /etc/resolv.conf)
              ,(nbind /nix/store /nix/store)
              ,(nbind-ro profile-bin profile-bin)
              ,(nbind locale-dir remote-locale-dir )
              ,(nbind dist-dir dist-dir)
              ,(nbind ./bin /opt/bin)
              ,iface
              ,setenv ,(param LOCAL_ARCHIVE remote-locale-dir)
              ,setenv ,(param LC_ALL C.utf8)
              ,setenv ,(param PATH remote-path)
              ,setenv ,(param PS1 (concat (join space "(nspawn) ")))
              /opt/bin/hbs2-cli --run /tmp/.env
            ])
    (map sym nargs0)
  )
)


(println "preparing dirs")
(rm local-dirs)
(mkdir local-dirs)
(touch env-file)
(call:proc :chmod :1777 env-file)

;;FIXME: interface hardcode

(vm-shell-write)
(run:proc:quiet sudo `[ip link delete ,local-net])
(sleep 0.3)
(run:proc:attached sudo `[ip link add ,local-net link ,local-phy type macvlan mode bridge])
(sleep 0.3)
(run:proc:attached sudo `[ip link set ,local-net up])
(run:proc:attached sudo `[ip addr add ,local-ip dev ,local-net])
(sleep 0.3)

(run:proc:attached sudo (cons systemd-nspawn [nspawn-args]))

(run:proc:attached sudo `[rm -rf ,local-temp-dir])
(run:proc:attached sudo `[ip link delete ,local-net])


