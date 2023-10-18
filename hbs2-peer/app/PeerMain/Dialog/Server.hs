{-# LANGUAGE PolyKinds #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module PeerMain.Dialog.Server where

import Codec.Serialise
import Control.Monad.Except
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Lens.Micro.Platform

import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Dialog.Core
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Types
import HBS2.Prelude
import HBS2.Storage.Simple

import PeerMain.Dialog.Spec

---

data DialEnv = DialEnv

newtype DialT m a = DialT { unDialT :: PeerM L4Proto (ReaderT DialEnv (DialHandlerT m)) a }
  deriving
    ( Generic, Functor, Applicative, Monad
    , MonadIO
    , MonadReader (PeerEnv L4Proto)
    -- , MonadTrans
    -- , MonadError ResponseStatus
    -- , MonadThrow, MonadCatch, MonadMask
    )

-- instance Monad m => MonadReader DialEnv (DialT m) where
--     ask = DialT . lift $ ask
--     local f ma = undefined

instance Monad m => HasStorage (DialT m) where
  getStorage = asks (view envStorage)

instance MonadTrans DialT where
  lift = DialT . lift . lift . lift

instance Monad m =>
  MonadError ResponseStatus (DialT m) where
    -- {-# MINIMAL throwError, catchError #-}
    -- throwError :: e -> m a
    throwError = DialT . lift . throwError
    -- catchError :: m a -> (e -> m a) -> m a
    catchError = undefined

---

runDialTtoDialHandlerT :: MonadIO m => DialEnv -> PeerEnv L4Proto -> DialT m a -> DialHandlerT m a
runDialTtoDialHandlerT denv penv =
    flip runReaderT denv . withPeerM penv . unDialT

---

dialogRoutes' :: forall m .
    ( MonadIO m
    , Serialise (PubKey 'Sign (Encryption L4Proto))
    , FromStringMaybe (PubKey 'Sign (Encryption L4Proto))
    , Hashable (PubKey 'Sign (Encryption L4Proto))
    , Pretty (AsBase58 (PubKey 'Sign (Encryption L4Proto)))
    )
    => PeerEnv L4Proto
    -> DialogRequestRouter m
dialogRoutes' penv = dialogRequestRoutes do

    hand ["ping"] \req -> (, req) <$> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus Success200 "") 0), "pong"])

    hand ["spec"] \req -> (, req) <$> Right \reply -> do
        undefined
        -- let xs = Map.keys (unDialogRequestRouter (dialogRoutes @m penv))

        -- forM_ (zip (zip [1..] xs) ((True <$ drop 1 xs) <> [False])) \((j,x),isMore) -> do
        --     reply (Frames [serialiseS (ResponseHeader (ResponseStatus (bool Success200 SuccessMore isMore) "") j)
        --               , BS.intercalate "/" x
        --               ])


    hand ["debug", "no-response-header"] \req -> (, req) <$> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "one"])
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 1), "two"])
        reply (Frames [])

    hand ["debug", "wrong-header"] \req -> (, req) <$> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "correct-header"])
        reply (Frames ["wrong-header"])

    hand ["debug", "timeout"] \req -> (, req) <$> Right \reply -> do
        reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessMore "") 0), "false more"])


    handconv ["reflog", "get"] "ReflogGetReq" \(ReflogGetReq ref) -> do

        sto <- withPeerM penv getStorage

        hash <- maybe (throwError (ResponseStatus NotFound404 "unknown reference")) pure
            =<< liftIO do
                    getRef sto (RefLogKey @(Encryption L4Proto) ref)

        pure (ReflogGetResp hash)

newtype ReflogGetReq = ReflogGetReq (PubKey 'Sign (Encryption L4Proto))
  deriving (Generic)

instance Serialise (PubKey 'Sign (Encryption L4Proto))
    => Serialise ReflogGetReq

newtype ReflogGetResp = ReflogGetResp (Hash HbSync)
  deriving (Generic)

instance Serialise (PubKey 'Sign (Encryption L4Proto))
    => Serialise ReflogGetResp

---

drpcFullDApp :: forall m .
    ( MonadIO m
    , Unconstraints
    )
    => DialEnv -> PeerEnv L4Proto -> DApp m
drpcFullDApp denv penv =
    mkDApp
        (Proxy @(NamedSpec DialogRPCSpec))
        EmptyCtx
        (runDialTtoDialHandlerT denv penv)
        -- (drpcFullH :: DialogRPCSpec (ModeServerT (DialT m)))
        drpcFullH

type ConstraintsH m =
    ( MonadIO m
    , MonadError ResponseStatus m
    , HasStorage m
    , Unconstraints
    )

type Unconstraints =
    ( Serialise (PubKey 'Sign (Encryption L4Proto))
    , Hashable (PubKey 'Sign (Encryption L4Proto))
    , Show (PubKey 'Sign (Encryption L4Proto))
    , Pretty (AsBase58 (PubKey 'Sign (Encryption L4Proto)))
    , Typeable (PubKey 'Sign (Encryption L4Proto))
    , FromStringMaybe (PubKey 'Sign (Encryption L4Proto))
    )

drpcFullH :: ( ConstraintsH m )
    => DialogRPCSpec (ModeServerT m)
drpcFullH = DialogRPCSpec
  { drpcPing = pure "pong"
  , drpcSpec = pure "tbd"
  , drpcReflog = reflogH
  }

reflogH :: ( ConstraintsH m )
    => RPCReflogSpec (ModeServerT m)
reflogH = RPCReflogSpec {..}
  where

    reflogGet ref = do

        sto <- getStorage

        hash <- maybe (throwError (ResponseStatus NotFound404 "unknown reference")) pure
            =<< liftIO do
                    getRef sto (RefLogKey @(Encryption L4Proto) ref)

        pure hash

    reflogFetch pk = do
        liftIO $ print pk
        pure ()

