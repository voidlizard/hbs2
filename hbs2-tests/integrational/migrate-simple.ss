(import ./hbs2-tests/integrational/tmux-env.ss)


(local image  /home/dmz/backup/peer.hbs2.net/hbs2.squashfs)

(local lo-device [head [flatten [call:proc sudo losetup --show -fP image]]])

(local mount-point (sys:temp:dir))

println mount-point

(println "LOOP" space lo-device)

(println "mounted" space image space "at" space mount-point)

(local overlay-root (sys:temp:dir))

(local overlay-upper (sym (join :/ overlay-root  :upper)))
(local overlay-work  (sym (join :/ overlay-root  :work)))
(local overlay-merged (sym (join :/ overlay-root :merged)))

(mkdir overlay-upper)
(mkdir overlay-work)
(mkdir overlay-merged)

(call:proc sudo mount -t squashfs -o ro lo-device mount-point)

(local mountopts
       (sym (concat :lowerdir= mount-point
               chr:comma
               :upperdir= overlay-upper
               chr:comma
               :workdir= overlay-work
       ))
)

(println mountopts)

(call:proc:raw sudo mount -t overlay overlay -o mountopts  overlay-merged)

(define *nspawn-extra-args
  `[ ,(nbind (join :/ overlay-merged blocks)
             /root/.local/share/hbs2/blocks)
     ,(nbind (join :/ overlay-merged refs)
             /root/.local/share/hbs2/refs)
   ])

(println *nspawn-extra-args)

(run-shell)

(call:proc sudo umount overlay-merged)
(call:proc sudo umount mount-point)
(call:proc sudo losetup -d lo-device)
(call:proc sudo :rm -rf  overlay-merged overlay-work overlay-root mount-point)



