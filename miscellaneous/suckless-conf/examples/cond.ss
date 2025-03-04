
(cond
  ((eq? 1 1) (print "1" :== "1"))
  (_ (print "none") )
)

(define a 42)

(newline)

(cond
  ((eq? a 2) (print "1" :== "1"))
  ((gt? a 5) (print "a > 5"))
  (_ (print "none") )
)

(newline)

(cond
  ((eq? a 2) (print "1" :== "1"))
  ((gt? a 100) (print "a > 100"))
  (_ (print "none") )
)

(newline)

(cond
  (#t "true")
  (_ "false" )
)

(cond
  ((not #t) "not true")
  (_ "false" )
)






