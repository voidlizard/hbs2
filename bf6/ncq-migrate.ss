
(define STORAGE (path:join (env HOME) .local/share/hbs2 ))
(define REFS    (path:join STORAGE  refs)  )
(define BLOCKS  (path:join STORAGE  blocks)  )
(define NEW     (path:join (env HOME) tmp/ncq0))


(define refs   (glob REFS '[*/**]  ))
(define blocks (glob BLOCKS '[*/**]  ))

(define (readref x)
  (begin
    (local ref    (concat (reverse (take 2 (reverse (split :/ x))))))
    (local refval (str:file x))
    `(,(sym ref) ,(sym refval))))

(define (readhash x)
  (sym (concat (reverse (take 2 (reverse (split :/ x))))))
)

(local zu (map readref refs))

; (println zu)

(println STORAGE)
(println NEW)

; debug
(define ncq (ncq:open NEW))

(define (writeref x)
  (match x
    ( (list? a b )
      (begin
        (ncq:set:ref ncq a b)
        (println ref space a space b)
      )
    )
    (_ '())
 ))

(define (import-refs) (for zu writeref))

(define (import-blocks)
  (begin
    ; (local (write x) (ncq:put ncq (bytes:file x)))
    (for blocks (fn x .
      (begin
        (local h0 (sym (readhash x)))
        (local here  (ncq:has ncq h0))
        (if (not here)
          (begin
            (local ha (sym (ncq:put ncq (bytes:strict:file x))))
            (local s  (coalesce "" (ncq:has ncq ha)))
            (local ok (if (eq? ha h0) (ansi :green _ ok) (ansi :red _ fail)))
            (println block space ok space (align -6 (str s)) space ha space h0 space )
            ; (println block space ok space space ha space h0 space )
            (if (not (eq? ha h0)) (die "*** block import error:" ha space h0)))

          (println "block" space (ansi :yellow _ "skip") space h0)
        )
    )))
  )
)

(import-blocks)
(import-refs)


; ; (println OKAY)


