(println "list/4")

(define (l4 s)
  (match s
     ( [list? a ...] (begin (println :l4 space a) (l4 ...)) )
     ( [list?]       (println :l4 space :done))
  )
)

(l4 '[1 2 3 4 5 6])

