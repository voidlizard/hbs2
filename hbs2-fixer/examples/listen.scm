;; hbs2-fixer config example

; (debug off)

(watch-config 30)

(local home (getenv "HOME"))

(local root (string-append home "/.local/share/hbs2-git-repos/0.24.1"))

(local hbs2-repo  "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP" )
(local hbs2-repo-path (string-append root "/" hbs2-repo))


(local myref "BKtvRLispCM9UuQqHaNxu4SEUzpQNQ3PeRNknecKGPZ6" )

(listen (reflog myref)
   (display (string-append "HELLO FROM REFLOG " (unquoted myref)))
)

(listen (lwwref myref)
   (display "WON'T HAPPEN")
)

(display "FUUBAR!")
