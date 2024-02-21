{-# Language UndecidableInstances #-}
module DispatchProxy
  ( RouteFun
  , newDispatchProxy
  , runDispatchProxy
  , Dispatched(..)
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging

import PeerLogger

import Control.Concurrent.STM.TQueue qualified as TQ
import Data.ByteString.Lazy (ByteString)
import Control.Monad

import UnliftIO

data Dispatched = forall bus . Messaging bus L4Proto ByteString => Dispatched bus

type RouteFun e = forall m . (MonadIO m)
                   => ByteString
                   -> Peer e
                   -> m (Maybe Dispatched)


data DispatchProxy  =
  DispatchProxy
  { points :: [ (Peer L4Proto, Dispatched) ]
  , route  :: RouteFun L4Proto
  , inbox  :: TQueue (From L4Proto, ByteString)
  }

newDispatchProxy :: (MonadIO m)
                 => [ (Peer L4Proto, Dispatched) ]
                 -> RouteFun L4Proto
                 -> m DispatchProxy

newDispatchProxy p r = DispatchProxy p r <$> newTQueueIO


runDispatchProxy :: forall  m . ( MonadUnliftIO m
                                )
                 => DispatchProxy
                 -> m ()

runDispatchProxy d = do
  debug "runDispatchProxy"

  ps <- for (points d) $ \(pip, Dispatched mess) -> async do
          forever do
            receive mess (To pip)
              >>= mapM_ (atomically . writeTQueue (inbox d))

  mapM_ link ps
  void $ waitAnyCatchCancel ps


instance Messaging DispatchProxy L4Proto ByteString where

  sendTo bus t@(To whom) f m = do
    route bus m whom >>= maybe none sendRouted
    where
      sendRouted (Dispatched target) = sendTo target t f m

  receive bus _ = do
    void $ atomically $ peekTQueue (inbox bus)
    liftIO $ atomically $ TQ.flushTQueue (inbox bus)

