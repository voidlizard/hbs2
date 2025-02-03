
[define source [json:file miscellaneous/fuzzy-parse/nix/pkgs.json]]

; iterate println source

print
[html :html [kw]
  [html :head [kw]
    [html :title [kw] "Suckless HTML Page"]
    [html :meta [kw :charset "UTF-8"]]
    [html :style [kw]
      [css body [kw font-family sans-serif margin 40px]]
      [css table [kw border-collapse "collapse width 100%"]]

      [css (list td th) [kw border "1px solid #ccc"
                            padding 8px
                            text-align left]]

      [css th [kw background-color #f2f2f2]]
      [css .che [kw margin-right 8px]]
    ]
  ]

  [html :body [kw]
    [html :h1 [kw] "Пример страницы"]
    [html :h2 [kw] "Сделано на Suckless Script"]

    [html :p [kw] "Это пример страницы, созданной в `hbs2`."]

    [html :form [kw action "#" method "POST"]
      [html :label [kw for "cb1"]
        [html :input [kw :type checkbox name :checkbox1 :id cb1 :class che]]
        "Согласен с условиями"
      ]
      [html :br]
      [html :input [kw :type text :name username :placeholder "Введите имя"]]
      [html :br]
      [html :input [kw :type submit :value "Отправить"]]
    ]

    [html :br]

    [html :p [kw]
      "Этот текст с "
      [html :b [kw] "жирным"]
      ", "
      [html :i [kw] "курсивом"]
      " и "
      [html :u [kw] "подчёркнутым"]
      " стилями."
    ]

    [html :br]

    [html :table [kw]
      [html :thead [kw]
        [html :tr [kw]
          [html [kw] :td]
          [html [kw] :td]
        ]
      ]

      [html :tbody [kw]
        [map [fn 1 [html :tr [kw] [html :th [kw] [car _1]]
                   [html :td [kw] [nth 1 _1]]  ] ] source]
      ]
    ]

    [html :br]

    [html :p [kw]
      Подробнее читайте на
      [html :a [kw href "http://example.com"] нашем сайте]
      "."
    ]
  ]
]

