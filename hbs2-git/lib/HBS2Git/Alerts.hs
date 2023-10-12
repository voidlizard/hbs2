module HBS2Git.Alerts where

import HBS2.Prelude

import Text.InterpolatedString.Perl6 (qc)

noKeyInfoMsg :: forall a . Pretty a => a -> String
noKeyInfoMsg repo =
  [qc|*** No KeyInfo found, maybe malformed 'encryption' section for {pretty repo} in config|]
