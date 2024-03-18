
;;

(watch 30 (lwwref "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
  (run "./on-my-ref.sh")
)

(watch 10 (lwwref "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
  (run "./on-my-ref2.sh")
)

(watch 10 (reflog "BKtvRLispCM9UuQqHaNxu4SEUzpQNQ3PeRNknecKGPZ6")
  (run "./on-my-ref3.sh")
)


