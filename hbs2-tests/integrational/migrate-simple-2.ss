(import ./hbs2-tests/integrational/tmux-env.ss)

(local real-root /home/dmz/w/hbs2/temp/real)

(define *nspawn-extra-args
  `[ ,(nbind (join :/ real-root blocks)
                /root/.local/share/hbs2/blocks)
     ,(nbind (join :/ real-root refs)
                /root/.local/share/hbs2/refs)
   ])

(println *nspawn-extra-args)

(run-shell)
