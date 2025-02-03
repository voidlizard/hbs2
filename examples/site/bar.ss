; это страница, на которую ссылается foo.ss

(define (bar-page)

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
   [html :h1 [kw] Some other page]
   [html :h2 [kw] Built with Suckless Script]

   [html :p [kw] This is an example page generated using hbs2.]

   Just some text

 ]]

)



