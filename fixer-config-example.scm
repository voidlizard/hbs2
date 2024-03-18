
;;

(watch 30 (lwwref "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
  (run "./on-my-ref2.sh")
)

(watch 30 (reflog "BKtvRLispCM9UuQqHaNxu4SEUzpQNQ3PeRNknecKGPZ6")
  (run "./on-my-ref3.sh")
)

(watch 30 (lwwref "DTmSb3Au7apDTMctQn6yqs9GJ8mFW7YQXzgVqZpmkTtf")
  (run "./on-my-ref4.sh")
)

(watch 10 (lwwref "Byc3XUeSbJBXVFueumkNkVJMPHbGoUdxYEJBgzJPf8io")
  (run "./on-my-ref4.sh")
)

