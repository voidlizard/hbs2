module HBS2Git.PrettyStuff where

import Prettyprinter
import Prettyprinter.Render.Terminal

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)


red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

section :: Doc ann
section = line <> line
