

(local l1 [list 1 2 3 4 5 6 7 8])


(define (q x) (begin (println x) (f x)))


(define (f x)
(match x
  ( [list? n ...] (q ...) )
  ( [list?] '()  )
))

(f l1)


