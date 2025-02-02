
(define h html)
(define (bold . co) [html b [kw] co])
(define (ul . co) [html :ul [kw] co] )
(define (td . co) [html :td [kw] co] )

(define files
 [sort-by [fn 1 [nth 8 _1]] [call:proc ls -la]]
)

print

  [h :html [kw]

    [h :head [kw] ]

    [h body [kw]
      [h h1 [kw] "jopa kita" ]
      [h br]

      [h p [kw] [bold current time:] (now)]

      [h p [kw]
        "МАМА МЫЛА РАМУ А У ПАПЫ ЗАПОЙ"
        [h a [kw href http://localhost:5000/] LOCALHOST ]
        [h br]

        Тут конечно надо все элементы пробивать пробелами,
        и это легко сделать!
      ]

      [ul
        [map [fn 1 [html li [kw] [car _1] ]  ] [grep NIX [env]]]
      ]

      [html :table [kw]
        [map [fn 1 [html :tr [kw] [td [nth 4 _1]] [td [nth 8 _1]] ] ] files ]
      ]

      ]
    ]


