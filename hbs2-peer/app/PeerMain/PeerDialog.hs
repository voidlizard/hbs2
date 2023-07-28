module PeerMain.PeerDialog where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.ByteString qualified as BS
import Data.Map qualified as Map

import Dialog.Core
import HBS2.Net.Proto.Types


dialogRoutes :: forall m . MonadIO m => DialogRequestRouter m
dialogRoutes = dialogRequestRoutes do

    hand ["ping"] \req -> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus Success200 "") 0), "pong"])

    hand ["spec"] \req -> Right \reply -> do
        let xs = Map.keys (unDialogRequestRouter (dialogRoutes @m))

        forM_ (zip (zip [1..] xs) ((True <$ drop 1 xs) <> [False])) \((j,x),isMore) -> do
            reply (Frames [serialiseS (ResponseHeader (ResponseStatus (bool Success200 SuccessMore isMore) "") j)
                      , BS.intercalate "/" x
                      ])


    hand ["debug", "no-response-header"] \req -> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "one"])
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 1), "two"])
        reply (Frames [])

    hand ["debug", "wrong-header"] \req -> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "correct-header"])
        reply (Frames ["wrong-header"])

    hand ["debug", "timeout"] \req -> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "false more"])

