(println test1)

(match 123
  ( [? foo [? k [int? _]] ] (println okay space k space foo))
  ( _ (println not-okay))
)

