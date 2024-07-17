{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.Misc.PrettyStuff as All
import HBS2.System.Logger.Simple.ANSI as All

import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer

import HBS2.Peer.Proto hiding (request)
import HBS2.Peer.Proto.RefLog
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Data.Types.SignedBox


import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.State
import HBS2.KeyMan.App.Types

import HBS2.Misc.PrettyStuff

import Data.Coerce
import Data.Config.Suckless
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.List (isPrefixOf)
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Maybe
import Codec.Serialise
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import UnliftIO
import System.Environment
import System.IO (hPrint)

import Streaming.Prelude qualified as S
import Prettyprinter

type RefLogId = PubKey 'Sign 'HBS2Basic

pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)

stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes

data BindAction c ( m :: Type -> Type)  =
    BindLambda { fromLambda :: [Syntax c] -> RunM c m (Syntax c) }
  | BindValue  (Syntax c)

data Bind c ( m :: Type -> Type)  = Bind
  { bindAction      :: BindAction c m
  , bindName        :: Id
  , bindDescShort   :: Text
  } deriving (Generic)

deriving newtype instance Hashable Id

newtype NameNotBoundException = NameNotBound Id
                            deriving stock Show
                            deriving newtype (Generic,Typeable)


newtype NotLambda = NotLambda Id
                    deriving stock Show
                    deriving newtype (Generic,Typeable)

instance Exception NotLambda

data BadFormException c = BadFormException (Syntax c)

newtype BadValueException = BadValueException String
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

instance IsContext c => Show (BadFormException c) where
  show (BadFormException sy) = show $ "BadFormException" <+> pretty sy

instance Exception (BadFormException C)

instance Exception BadValueException

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
    go acc ( x : rest ) | isPrefixOf "-"  x =  go ( x : acc ) rest
    go acc ( x : rest ) | isPrefixOf "--" x =  go ( x : acc ) rest
    go acc ( x : rest ) = go ( x : acc ) rest
    go acc [] = emit acc

    emit = S.yield . reverse


run :: forall c m . ( IsContext c
                    , MonadUnliftIO m
                    , Exception (BadFormException c)
                    ) => Dict c m -> [Syntax c] -> m (Syntax c)
run d sy = do
  tvd <- newTVarIO d
  lastDef nil <$> runReaderT (fromRunM (mapM runExpr sy)) tvd
  where
    runExpr :: Syntax c -> RunM c m (Syntax c)
    runExpr syn = handle (handleForm syn) $ case syn of
      ListVal (SymbolVal name : args') -> do
        what <- ask >>=  readTVarIO <&> HM.lookup name . fromDict
        case bindAction <$> what of
          Just (BindLambda e) -> mapM runExpr args' >>= e
          Just (BindValue v)  -> throwIO (NotLambda name)
          Nothing -> throwIO (NameNotBound name)

      SymbolVal (Id s) | Text.isPrefixOf ":" s -> do
        pure (mkSym @c (Text.drop 1 s))

      SymbolVal name -> do
        what <- ask >>=  readTVarIO
                  <&> HM.lookup name . fromDict
                  <&> maybe (BindValue (mkSym name)) bindAction

        case what of
          BindValue e  -> pure e
          BindLambda e -> pure $ mkForm "lambda" [mkSym "..."]

      e -> pure e

    handleForm syn (BadFormException _  :: BadFormException c) = do
      throwIO (BadFormException syn)

bindMatch :: Id -> ([Syntax c] -> RunM c m (Syntax c)) -> Dict c m
bindMatch n fn = Dict (HM.singleton n (Bind (BindLambda fn) n ""))

nil :: forall c . IsContext c => Syntax c
nil = List noContext []

nil_ :: (IsContext c, MonadIO m) =>  (a -> RunM c m b) -> a -> RunM c m (Syntax c)
nil_ m w = m w >> pure (List noContext [])

bind :: (MonadUnliftIO m, IsContext c) => Id -> Syntax c -> RunM c m (Syntax c)
bind name expr = do
  tv <- ask -- >>=  readTVarIO
  atomically do
    w@(Dict x) <- readTVar tv
    writeTVar tv w
  pure nil

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

display :: (MonadIO m, Pretty a) => a -> m ()
display = liftIO . print . pretty

display_ :: (MonadIO m, Show a) => a -> m ()
display_ = liftIO . print

{- HLINT ignore "Functor law" -}

class IsContext c => MkSym c a where
  mkSym :: a -> Syntax c

instance IsContext c => MkSym c String where
  mkSym s = Symbol noContext (Id $ Text.pack s)

instance IsContext c => MkSym c Text where
  mkSym s = Symbol noContext (Id s)

instance IsContext c => MkSym c Id where
  mkSym = Symbol noContext

mkStr :: forall c . IsContext c => String -> Syntax c
mkStr s = Literal noContext $ LitStr (Text.pack s)

mkForm :: forall c . IsContext c => String -> [Syntax c] -> Syntax c
mkForm s sy = List noContext ( mkSym s :  sy )

mkList :: [Syntax C] -> Syntax C
mkList = List noContext

getCredentialsForReflog :: MonadUnliftIO m => String -> m (PeerCredentials 'HBS2Basic)
getCredentialsForReflog reflog = do
  puk <- orThrow (BadValueException reflog) (fromStringMay @(RefLogKey HBS2Basic) reflog)
  runKeymanClient (loadCredentials puk)
     >>= orThrowUser "credentials not found"

mkRefLogUpdateFrom :: MonadUnliftIO m => m ByteString -> String -> m (Syntax C)
mkRefLogUpdateFrom mbs  reflog = do
  what <- getCredentialsForReflog reflog
  let puk = view peerSignPk what
  let privk = view peerSignSk what
  txraw <- mbs
  w <- makeRefLogUpdate @L4Proto @'HBS2Basic (coerce puk) privk txraw
  let s = show $ pretty $ AsBase58 (serialise w)
  pure $ mkForm "cbor:base58" [ mkStr s ]


main :: IO ()
main = do

  setupLogger

  cli <- getArgs <&> unlines . fmap unwords . splitForms
           >>= either (error.show) pure . parseTop

  let dict = execWriter do

        tell $ bindMatch "help" $ nil_ \case
           [] -> do
            d <- ask >>= readTVarIO <&> fromDict
            mapM_ (display.bindName) d

           _ -> pure ()

        tell $ bindMatch "concat" $ \case
          StringLikeList xs@(_:_) -> do
            pure $ mkStr ( mconcat xs )
          _ -> throwIO (BadFormException @C nil)


        tell $ bindMatch "lookup" $ \case
          [StringLike s, ListVal (SymbolVal "dict" : es) ] -> do
            let val = headDef nil [ v | ListVal [StringLike k, v] <- es, k == s ]
            pure val

          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "display" $ nil_ \case
          [ sy ] -> display sy
          ss     -> display (mkList ss)

        tell $ bindMatch "internal:show-cli" $ nil_ \case
          _ -> display cli

        tell $ bindMatch "hbs2:peer:detect" $ nil_ \case
          _ -> do
            so <- detectRPC
            display so

        tell $ bindMatch "hbs2:peer:poke" $ \case
          _ -> do
            so <- detectRPC `orDie` "hbs2-peer not found"
            r <- newTVarIO nil
            withRPC2 @PeerAPI  @UNIX so $ \caller -> do

              what <- callRpcWaitMay @RpcPoke (TimeoutSec 1) caller ()
                        <&> fromMaybe ""
                        <&> parseTop
                        <&> either (const nil) (mkForm "dict")

              atomically $ writeTVar r what

            readTVarIO r

        tell $ bindMatch "hbs2:keyman:list" $ nil_ \case
          _ -> do
            void $ runKeymanClient  $ KeyManClient $ do
              k <- listKeys
              display_ $ vcat (fmap pretty k)

        tell $ bindMatch "hbs2:reflog:tx:create-raw" $ \case
          [SymbolVal "stdin", StringLike reflog] -> do
            mkRefLogUpdateFrom ( liftIO BS.getContents ) reflog

          [LitStrVal s, StringLike reflog] -> do
            mkRefLogUpdateFrom ( pure (TE.encodeUtf8 s) ) reflog

          _ -> throwIO (BadFormException @C nil)

  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- getContents
                >>= either (error.show) pure . parseTop
      void $ run dict what

    _ -> do
      void $ run dict cli

