[define source [json:file miscellaneous/fuzzy-parse/nix/pkgs.json]]

print
[html :html [kw]
  [html :head [kw]
    [html :title [kw] Suckless HTML Page]
    [html :meta [kw :charset UTF-8]]
    [html :style [kw]
      [css body [kw font-family sans-serif margin-left 20px max-width 1024px]]
      [css table [kw border-collapse collapse width auto]]

      [css (list td th) [kw border [sym (unwords 1px solid #ccc)]
                            padding 8px
                            text-align left]]

      [css th [kw background-color #f2f2f2 white-space nowrap]]
      [css .che [kw margin-right 8px]]
    ]
  ]

  [html :body [kw]
    [html :h1 [kw] Example Page]
    [html :h2 [kw] Built with Suckless Script]

    [html :p [kw] This is an example page generated using hbs2.]

    [html :form [kw action # method POST]
      [html :label [kw for cb1]
        [html :input [kw :type checkbox name checkbox1 :id cb1 :class che]]
        I agree with the terms
      ]
      [html :br]
      [html :input [kw :type text :name username :placeholder "Enter your name"]]
      [html :br]
      [html :input [kw :type submit :value Submit]]
    ]

    [html :br]

    [html :p [kw]
      This text contains

      [html :b [kw] bold]

      chr:comma

      [html :i [kw] italic]

      :and

      [html :u [kw] :underlined]

      styles.
    ]

    [html :br]

    ; Unicode test section
    [html :p [kw] Russian: Привет, мир!]
    [html :p [kw] Chinese: 你好，世界！]
    [html :p [kw] Korean: 안녕하세요, 세계!]

    [html :br]

    [html :table [kw]
      [html :thead [kw]
        [html :tr [kw]
          [html :th [kw] Package]
          [html :th [kw] Version]
        ]
      ]

      [html :tbody [kw]
        [map [fn 1 [html :tr [kw] [html :th [kw] [car _1]]
                   [html :td [kw] [nth 1 _1]]  ] ] source]
      ]
    ]

    [html :br]

    [html :p [kw]
      For more information, visit
      [html :a [kw href http://example.com] our website]
      "."
    ]
  ]
]

