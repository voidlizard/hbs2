;; hbs2-fixer config example

(local home (getenv "HOME"))

(local root (string-append home "/.local/share/hbs2-git-repos/0.24.1"))

(local hbs2-repo  "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP" )
(local hbs2-repo-path (string-append root "/" hbs2-repo))

(watch-config 30)

(debug off)

(display (string-append "repo1" " " hbs2-repo-path))

(eval (list (display "OKAY11 FROM EVAL")))

(on-start
  (display (string-append "on-start" " " "mkdir" " " hbs2-repo-path))

  (mkdir hbs2-repo-path)

  (run (string-append "git init --bare " hbs2-repo-path))
  (display update-hbs2-repo)

  (run (list opts (cwd hbs2-repo-path))
       (string-append "git hbs2 import" " " hbs2-repo))

  (run (list opts (cwd hbs2-repo-path))
       (string-append "git gc" ) )
)

(watch 60 (lwwref "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
  (run-config
    (watch 300 (lwwref:get-hbs2-git-reflog)
       (display "GIT REFLOG CHANGED BY WATCH")

       (run (list opts (cwd hbs2-repo-path))
            (string-append "git hbs2 import" " " hbs2-repo ))

       (display (string-append "Updated " hbs2-repo " OK"))

    )

    (listen (lwwref:get-hbs2-git-reflog)

       (display "GIT REFLOG CHANGED BY LISTENER")

       (run (list opts (cwd hbs2-repo-path))
            (string-append "git hbs2 import" " " hbs2-repo ))

       (display (string-append "Updated " hbs2-repo " OK"))
    )

    )
  (display (string-append "Updated " hbs2-repo))
)

; (watch 30 (lwwref "Byc3XUeSbJBXVFueumkNkVJMPHbGoUdxYEJBgzJPf8io")
;   (run "./on-my-ref4.sh")
; )

; (watch 30 (lwwref "DTmSb3Au7apDTMctQn6yqs9GJ8mFW7YQXzgVqZpmkTtf")
;   (run "./on-my-ref4.sh")
; )

; (watch 30 (reflog "BKtvRLispCM9UuQqHaNxu4SEUzpQNQ3PeRNknecKGPZ6")
;   (run "./on-my-ref4.sh")
; )

; (display "JOPAKITA 111")

