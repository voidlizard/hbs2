module HBS2Git.PrettyStuff
  ( module HBS2Git.PrettyStuff
  , hPutDoc
  ) where

import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Terminal

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)

section :: Doc ann
section = line <> line

toStringANSI :: Doc AnsiStyle -> String
toStringANSI doc = Text.unpack $ renderStrict $ layoutPretty defaultLayoutOptions doc


-- asHex ::

