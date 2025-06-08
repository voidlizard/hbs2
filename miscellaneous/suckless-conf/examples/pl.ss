(println "list/1")

(println
  (match '[1 2 3 4]
    ( [list? ...] [display ok ...])
    ( _ fail-not-matched)
  )
)

(println "list/1.1")

(println
  (match [list 1 [list 2 3 4]]
    ( [list? a [list? b c ...] ...] [println a space c])
    ( _ fail-not-matched)
  )
)

