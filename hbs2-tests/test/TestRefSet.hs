module Main where

import HBS2.Prelude
import HBS2.Net.Proto.Ref
import HBS2.Data.Types.Refs

import Text.InterpolatedString.Perl6 (qc)
import Test.Tasty.HUnit

-- data RefACB = RefACBStr  String
--             | RefACBFile FilePath
--             | RefACBHash HashRef

-- newtype ACB


class MonadIO m => IsRef a e m where
  createRef :: a -> m (Ref e)

refDefScript :: String
refDefScript = [qc|

define-ref g1

ref-nonce g1 "bcc72c0f-98ac-4938-a5b4-aa5e2c2f2027"

; ref-acb-ref g1 "hash"
; ref-acb-id  g1 ""

ref-data g1 "hash"

|]


main = do
  pure ()
