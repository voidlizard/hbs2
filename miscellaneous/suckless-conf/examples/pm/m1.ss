
(define foo1 '[1 2 yeah])
(define foo2 '[a b [2 33] 45])

(define p1 '(list? _ _ [list? _ e ...] [? n [int? [rcurry gt? 20]]] ...))
(define p2 '(list? _ _ [list? _ e ...] [? n [int? [rcurry eq? 45]]] ...))
(define p3 '(list? _ _ [list? _ e ...] [? n [int? _]] ...))
(define p4 '(list? _ _ [list? _ e ...] [? n [int? 45]] ...))
(define p5 '(list? _ _ [list? _ e ...] [? n [int? 26]] ...))

(match foo1
  ( (list? _ _ k) (print "3-list" space k) )
  ( _ (print "whatever") )
)

(newline)

(match foo2
  ( p1 (print "found something" space e space n) )
  ( _ (print "whatever") )
)

(newline)

(match foo2
  ( p2 (print "found something" space e space n) )
  ( _ (print "whatever") )
)

(newline)

(match foo2
  ( p3 (print "found something" space e space n) )
  ( _ (print "whatever") )
)

(newline)

(match foo2
  ( p4 (print "found something" space e space n) )
  ( _ (print "whatever") )
)

(newline)

(match foo2
  ( p5 (print "found something" space e space n) )
  ( _ (print "whatever") )
)

(newline)

(match 100
  ( [int? _] (print okay) )
  ( _  (print not-okay) )
)

(newline)

(match 100
  ( [? a [int? _] ] (print okay :: a) )
  ( _  (print not-okay) )
)

(newline)

(match :aaa
  ( [? a [int? _] ] (print not-okay :: a) )
  ( _  (print okay) )
)


