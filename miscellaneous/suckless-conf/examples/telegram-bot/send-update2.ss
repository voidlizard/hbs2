import (sym [env HOME] /dmz-data/telegram/resources.ss)

; (println hbs2-bot-debug)

(define bot-id hbs2-bot-id)

(define (send-debug-message r . mess)
  (begin
    (local base      https://api.telegram.org/)
    (local bot       (concat base bot-id))
    (local url       (concat bot :/ :sendMessage))
    (local chat-id   (lookup:uw :chat r))
    (local thread-id (lookup:uw :thread r))
    (local (pp k v)  (concat k := v))
    (local msg  (unwords mess))
    (eval
      `(run:proc:attached
         curl -X POST ,url
              -d ,(pp :chat_id chat-id)
              ,@(if thread-id (-d . ,(pp :message_thread_id thread-id)))
              -d ,(pp :text msg)
       ))
    )
)



