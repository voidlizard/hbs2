
(println Test1)
(match 100
  ( [int? 100] (println okay))
  ( _ (println not-okay))
)

(println Test2)
(match 100
  ( [int?] (println okay))
  ( _ (println not-okay))
)

(println Test3)
(match 100
  ( [int? _] (println okay))
  ( _ (println not-okay))
)


