{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language FunctionalDependencies #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.CLI

import Data.Config.Suckless
import System.Environment
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Kind
import Control.Monad.State
import Control.Monad.Writer

import Prettyprinter


data BindAction c m = BindAction { getAction :: [Syntax c] -> m (Syntax c) }

data BindMeta c m = BindMeta
  { bindAction      :: BindAction c m
  , bindName        :: Id
  , bindDescShort   :: Text
  } deriving (Generic)

deriving newtype instance Hashable Id

class IsContext c => Bindable c a m  where
  bind :: Id -> a -> [BindMeta c m]

newtype PM c m = PM { unPM :: [Syntax c] -> m (Syntax c) }

instance (MonadIO m, IsContext c) => Bindable c (PM c m) m where
  bind i (PM f) = [BindMeta (BindAction f) i ""]

run :: forall c m . (IsContext c, MonadIO m) => [BindMeta c m] -> [Syntax c] -> m ()
run  meta syn = do
  let binds = [ (bindName x,x) | x <- meta ] & HM.fromList
  mapM_ (runExpr binds) syn

  where
    runExpr b = \case
      ListVal (SymbolVal what : args) -> do
        args' <- for args $ \a -> runExpr b a
        let fn = HM.lookup what b <&> bindAction
        case fn of
          Just (BindAction e) -> e args'
          _ -> error "not matched"
      _ -> error "not matched"

nil = List (noContext @C) []

main :: IO ()
main = do
  wat <- getContents
           >>= either (error.show) pure . parseTop

  let i = execWriter do
            tell $ bind  "hello" $ PM $ \case
              [SymbolVal s] -> print ("hello" <+> pretty s) >> pure nil
              [LitStrVal s] -> print ("hello" <+> pretty s) >> pure nil
              _ -> pure nil
            tell $ bind  "fuck" $ PM $ \case
              [] -> print "FUCK" >> pure nil
              _ -> pure nil

  run i wat


