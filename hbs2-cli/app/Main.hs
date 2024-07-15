{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie

import Data.Config.Suckless
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Control.Monad.Reader
import Control.Monad.Writer

import Prettyprinter

data BindAction c ( m :: Type -> Type)  = BindAction { getAction :: [Syntax c] -> RunM c m (Syntax c) }

data Bind c ( m :: Type -> Type)  = Bind
  { bindAction      :: BindAction c m
  , bindName        :: Id
  , bindDescShort   :: Text
  } deriving (Generic)

deriving newtype instance Hashable Id

newtype Dict c m = Dict { fromDict :: HashMap Id (Bind c m) }
                   deriving newtype (Semigroup, Monoid)

newtype RunM c m a = RunM { fromRunM :: ReaderT (Dict c m) m a }
                     deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (Dict c m))

run :: forall c m . (IsContext c, MonadIO m) => Dict c m -> [Syntax c] -> m ()
run d sy = do
  runReaderT (fromRunM (mapM_ runExpr sy)) d
  where
    runExpr :: Syntax c -> RunM c m (Syntax c)
    runExpr = \case
      ListVal (SymbolVal name : args') -> do
        what <- asks (HM.lookup name . fromDict) `orDie` "JOPA"
        case bindAction what of
          BindAction e -> do
            mapM runExpr args' >>= e

      e -> pure e

bindOne :: Id -> ([Syntax c] -> RunM c m (Syntax c)) -> Dict c m
bindOne n fn = Dict (HM.singleton n (Bind (BindAction fn) n ""))

nil = List noContext []

main :: IO ()
main = do
  wat <- getContents
           >>= either (error.show) pure . parseTop

  let dict = execWriter do
              tell $ bindOne "jopa" $ \case
                 [] -> liftIO (print "JOPA") >> pure nil
                 _ -> pure nil

              tell $ bindOne "help" $ \case
                 [] -> do
                  d <- asks (HM.keys . fromDict)
                  liftIO $ mapM_ (print.pretty) d
                  pure nil

                 _ -> pure nil

  run dict wat

