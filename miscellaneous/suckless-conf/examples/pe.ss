
(println

  (match 10
    ( n  n )
  )

)


(println

  (match 10
    ( _  ok )
  )

)


(println

  (match '[]
    ( [list?]  ok-nil )
  )
)

(println

  (match 11
    ( [int?]  ok-int )
  )
)

(println "atom/int")
(println

  (match :wtf
    ( [int?]  ok-int )
    ( _ ok-not-matched )
  )
)

(println "atom/atom")
(println

  (match :wtf
    ( [sym?]  ok-matched )
    ( _ fail-not-matched )
  )
)


(println "int/predicate")
(println

  (match 9000
    ( [int? 9000]  ok-matched-9000 )
    ( _ fail-not-matched )
  )
)

(println "int/predicate-2")
(println

  (match 9000
    ( [int? 5000]  fail-matched-5000 )
    ( _ ok-not-matched )
  )
)

(println "int/predicate-3")
(println

  (match 9000
    ( [int? [fn x . gt? x 1000]] ok-predicate-matched-gt-1000 )
    ( _ fail-not-matched )
  )
)

(println "int/predicate-4")
(println

  (match 9000
    ( [int? [fn x . le? x 1000]] fail-predicate-matched-le-1000 )
    ( _ ok-not-matched )
  )
)

(println "list/1")

(println
  (match '[1 2 3 4]
    ( [list? ...] [display ok ...])
    ( _ fail-not-matched)
  )
)


(println "list/2")

(println
  (match '[1 2]
    ( [list? a b] `[ok ,a ,b])
    ( _ fail-not-matched)
  )
)


(println "list/3")

(println
  (match '[1 2]
    ( [list? ...] [display ...])
    ( _ fail-not-matched)
  )
)



(println "list/3.1")

(println
  (match '[]
    ( [list? ...] (display ok ...) )
    ( _ fail-not-matched)
  )
)

(println "list/4")

(define (l4 s)
  (match s
     ( [list? a ...] (begin (println :l4 space a) (l4 ...)) )
     ( [list?]       :done)
  )
)

(l4 '[1 2 3 4 5])

(println :here-is-a space a)

(define bebebe 134)

(define (fuu) (define bebebe :jopa))

(fuu)

(println bebebe)








