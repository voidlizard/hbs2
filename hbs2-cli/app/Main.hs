{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.State
import HBS2.KeyMan.App.Types

import Data.Config.Suckless
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import UnliftIO
import System.Environment

import Streaming.Prelude qualified as S
import Prettyprinter

data BindAction c ( m :: Type -> Type)  = BindAction { getAction :: [Syntax c] -> RunM c m (Syntax c) }

data Bind c ( m :: Type -> Type)  = Bind
  { bindAction      :: BindAction c m
  , bindName        :: Id
  , bindDescShort   :: Text
  } deriving (Generic)

deriving newtype instance Hashable Id

newtype NameNotBoundException = NameNotBound Id
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

newtype Dict c m = Dict { fromDict :: HashMap Id (Bind c m) }
                   deriving newtype (Semigroup, Monoid)

newtype RunM c m a = RunM { fromRunM :: ReaderT (TVar (Dict c m)) m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadReader (TVar (Dict c m))
                                      )


splitForms :: [String] -> [[String]]
splitForms s0 = runIdentity $ S.toList_ (go mempty s0)
  where
    go acc ( "then" : rest ) = emit acc >> go mempty rest
    go acc ( "and" : rest ) = emit acc >> go mempty rest
    go acc ( x : rest ) = go ( x : acc ) rest
    go acc [] = emit acc

    emit = S.yield . reverse


run :: forall c m . (IsContext c, MonadIO m) => Dict c m -> [Syntax c] -> m ()
run d sy = do
  tvd <- newTVarIO d
  runReaderT (fromRunM (mapM_ runExpr sy)) tvd
  where
    runExpr :: Syntax c -> RunM c m (Syntax c)
    runExpr = \case
      ListVal (SymbolVal name : args') -> do
        what <- ask >>=  readTVarIO <&> HM.lookup name . fromDict
        case bindAction <$> what of
          Just (BindAction e) -> mapM runExpr args' >>= e
          Nothing -> throwIO (NameNotBound name)

      e -> pure e

bindOne :: Id -> ([Syntax c] -> RunM c m (Syntax c)) -> Dict c m
bindOne n fn = Dict (HM.singleton n (Bind (BindAction fn) n ""))

-- nil = List noContext []

nil_ :: (IsContext c, MonadIO m) =>  (a -> RunM c m b) -> a -> RunM c m (Syntax c)
nil_ m w = m w >> pure (List noContext [])

main :: IO ()
main = do

  cli <- getArgs <&> unlines . mconcat . splitForms
           >>= either (error.show) pure . parseTop

  let dict = execWriter do

        tell $ bindOne "help" $ nil_ $ \case
           [] -> do
            d <- ask >>= readTVarIO <&> fromDict
            liftIO $ mapM_ (print.pretty.bindName) d

           _ -> pure ()

        tell $ bindOne "internal:show-cli" $ nil_ $ \case
          _ -> liftIO (print $ pretty cli)

        tell $ bindOne "hbs2:peer:detect" $ nil_ $ \case
          _ -> do
            so <- detectRPC
            liftIO (print $ pretty so)

        tell $ bindOne "hbs2:peer:poke" $ nil_ $ \case
          _ -> do
            so <- detectRPC `orDie` "hbs2-peer not found"
            withRPC2 @PeerAPI  @UNIX so $ \caller -> do
              what <- callRpcWaitMay @RpcPoke (TimeoutSec 1) caller ()
              liftIO $ print $ pretty what

        tell $ bindOne "hbs2:keyman:list" $ nil_ $ \case
          _ -> do
            void $ runKeymanClient  $ KeyManClient $ do
              k <- listKeys
              liftIO $ print $ vcat (fmap pretty k)

  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- getContents
                >>= either (error.show) pure . parseTop
      run dict what

    _ -> do
      run dict cli

