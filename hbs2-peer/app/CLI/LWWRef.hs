module CLI.LWWRef where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.LWWRef

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import Options.Applicative

pLwwRef :: Parser (IO ())
pLwwRef = hsubparser (   command "fetch" (info pLwwRefFetch (progDesc "fetch lwwref"))
                      <> command "get" (info pLwwRefGet (progDesc "get lwwref"))
                     )
pLwwRefFetch :: Parser (IO ())
pLwwRefFetch = pure do
  pure ()

pLwwRefGet :: Parser (IO ())
pLwwRefGet = pure do
  pure ()


