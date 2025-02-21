
;; all key ids are PUBLIC

(define hosts
   ;; host-id sign-key encrypt-key
  `[
     (minime  4Z1ebkksoiZ9j4vZE9jnghxPDmc1ihXdNC6cX39phkLD
              9Fp8Y5c9Fp612sjby3bL8P3SnUjjK2bz4F38nmVASpzb)

     (expert  CxJaFMBykhTdUiXxgdWF2pjxV5cWtw3yjDozNniUYRRC
              Hg6XD19KGQrVjMYrCNeuaGfhTn7BCCUGR8c3brSWnzQi)

     (minipig 44onTKSrAjXQ42Ahu6Z8d5X35g23pTTbSgRudNow9ZEn
              D17PC8RGELG2wvTUoeAVhZvpf5R2txQHdwtYxGAJ9M1h)

   ]
)

(define (sign-key host)
  (str (nth 1 (assoc host hosts))))

(define (encrypt-key host)
  (str (nth 2 (assoc host hosts))))

(define my-refchan-head
  `[

(version 2)
(quorum 1)
(wait 10)
(peer "CVDMz8BiSvRsgWNbJ4u9vRwXthN8LoF8XbbbjoL2cNFd" 1)
(peer "5GnroAC8FXNRL8rcgJj6RTu9mt1AbuNd5MZVnDBcCKzb" 1)
(peer "J8dFP5TbUQxUpVbVqZ3NKKPwrhvUCTQKC6xrVWUGkrR6" 1)
(author ,(sign-key minime))
(author ,(sign-key expert))
(author ,(sign-key minipig))

(reader ,(encrypt-key minime))
(reader ,(encrypt-key expert))
(reader ,(encrypt-key minipig))
   ]
)

(define (create-refchan)
  [hbs2:refchan:create my-refchan-head]
)

;; created once by create-refchan
(define REFCHAN :Aze8PNNexhfz629UfaE79oyRW8Rf7fTGSVoJW4qD95Z7)

(define (update-refchan)
  [hbs2:refchan:head:update REFCHAN my-refchan-head]
)

(define (create:name:update host)
  (begin
    (local pk (sign-key host))
    (local tx (hbs2:refchan:tx:raw:create pk [unwords :name host]))
    tx
  )
)

(define (post:name:update refchan host)
  (begin
    (local tx (create:name:update host))
    (hbs2:refchan:tx:propose refchan tx)
  )
)

(define (state:get)
  (begin
    (local self [sym [car [cdr [car [ grep peer-key [hbs2:peer:poke] ]]]]])
    (local txs (grep :propose (hbs2:refchan:tx:raw:list REFCHAN)))
    (local (hostname e)  (car (cdr (car (top:string (bytes:decode [nth 4 _1]))))) )
    (local peers (call:proc hbs2-peer do peer-info))
    peers
    ; self
    ; (map [fn 1 [list [nth 2 _1] [hostname _1] ]] txs)
  )
)


