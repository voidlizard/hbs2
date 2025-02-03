
; [eval [cons :begin [top:file bar.ss]]]

(import bar.ss)
(import foo.ss)

(define site-root-ref :4X65y4YvUjRL2gtA9Ec3YDDP4bnxjTGhfjpoah96t3z1)

(define (as-html n) [kw :file-name n :mime-type "text/html; charset=utf-8"]) ; метаданные что бы hbs2-peer отображал как вебстраницу

(define bar.html (bar-page)) ; генерим страничку

(define bar.hash (hbs2:tree:metadata:string [as-html :bar.html] bar.html)) ; сохраняем как дерево с метаданными

(define foo.html (foo-page bar.hash))

(define foo.hash (hbs2:tree:metadata:string [as-html :foo.html] foo.html)) ; сохраняем как дерево с метаданными

(define grove [hbs2:grove:annotated [kw webroot foo.hash] [list foo.hash bar.hash]])

; println :bar.html space "hash:" space bar.hash

println Grove: space grove ; hello.hash

hbs2:lwwref:update site-root-ref grove

; newline

; print [hbs2:lwwref:get site-root-ref]

(define url [sym [join / http://localhost:5000/ref site-root-ref]]) ; вычисляем url

; newline

; print url

; print bar.html

; print foo.html

; print site-root-ref

(call:proc "firefox" url) ; вызываем фарфокс

