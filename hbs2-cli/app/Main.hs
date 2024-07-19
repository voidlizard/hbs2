{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Misc.PrettyStuff as All
import HBS2.System.Logger.Simple.ANSI as All

import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

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

import Data.Coerce
import Data.Config.Suckless
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Either
import Data.Maybe
import Codec.Serialise
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
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

class OptionalVal c b where
  optional :: b -> Syntax c -> b

instance IsContext c => OptionalVal c Int where
  optional d = \case
    LitIntVal x -> fromIntegral x
    _           -> d

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

newtype TypeCheckError c = TypeCheckError (Syntax c)

instance Exception (TypeCheckError C)

newtype BadValueException = BadValueException String
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

instance IsContext c => Show (BadFormException c) where
  show (BadFormException sy) = show $ "BadFormException" <+> pretty sy

instance IsContext c => Show (TypeCheckError c) where
  show (TypeCheckError sy) = show $ "TypeCheckError" <+> pretty sy

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

apply :: forall c m . ( IsContext c
                      , MonadUnliftIO m
                      , Exception (BadFormException c)
                      )
      => Id
      -> [Syntax c]
      -> RunM c m (Syntax c)
apply name args' = do
  what <- ask >>=  readTVarIO <&> HM.lookup name . fromDict
  case bindAction <$> what of
    Just (BindLambda e) -> mapM runExpr args' >>= e
    Just (BindValue v)  -> throwIO (NotLambda name)
    Nothing -> throwIO (NameNotBound name)


runExpr :: forall c m . ( IsContext c
                        , MonadUnliftIO m
                        , Exception (BadFormException c)
                        ) => Syntax c -> RunM c m (Syntax c)
runExpr syn = handle (handleForm syn) $ case syn of

  ListVal [ w, SymbolVal ".", b] -> do
    pure  $ mkList  [w, b]

  ListVal (SymbolVal name : args') -> do
    apply name args'

  SymbolVal (Id s) | Text.isPrefixOf ":" s -> do
    pure (mkSym @c (Text.drop 1 s))

  SymbolVal name -> do
    what <- ask >>=  readTVarIO
              <&> HM.lookup name . fromDict
              <&> maybe (BindValue (mkSym name)) bindAction

    case what of
      BindValue e  -> pure e
      BindLambda e -> pure $ mkForm "lambda" [mkSym name, mkSym "..."]

  e -> pure e

  where
    handleForm syn = \case
      (BadFormException _  :: BadFormException c) -> do
        throwIO (BadFormException syn)

run :: forall c m . ( IsContext c
                    , MonadUnliftIO m
                    , Exception (BadFormException c)
                    ) => Dict c m -> [Syntax c] -> m (Syntax c)
run d sy = do
  tvd <- newTVarIO d
  lastDef nil <$> runReaderT (fromRunM (mapM runExpr sy)) tvd

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
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
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

class Display a where
  display :: MonadIO m => a -> m ()

instance  {-# OVERLAPPABLE #-} Pretty w => Display w  where
  display = liftIO . print . pretty

instance Display (Syntax c) where
  display = \case
    LitStrVal s -> liftIO $ TIO.putStr s
    x           -> liftIO $ putStr (show $ pretty x)

instance Display Text where
  display = liftIO . TIO.putStr

instance Display String where
  display = liftIO . putStr

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

class IsContext c => MkStr c s where
  mkStr :: s -> Syntax c

instance IsContext c => MkStr c String where
  mkStr s = Literal noContext $ LitStr (Text.pack s)

instance IsContext c => MkStr c Text where
  mkStr s = Literal noContext $ LitStr s

mkForm :: forall c . IsContext c => String -> [Syntax c] -> Syntax c
mkForm s sy = List noContext ( mkSym s :  sy )

mkList :: forall c. IsContext c => [Syntax c] -> Syntax c
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


metaFromSyntax :: [Syntax c] -> HashMap Text Text
metaFromSyntax syn =
  HM.fromList [ (t k, t v) | (ListVal [ k, v ]) <- syn ]
  where
    t x = Text.pack (show $ pretty x)

createTreeWithMetadata :: (MonadUnliftIO m)
                       => HashMap Text Text
                       -> LBS.ByteString
                       -> m HashRef
createTreeWithMetadata meta lbs = do
    debug   "create fucking metadata"
    -- TODO: set-hbs2-peer
    so <- detectRPC `orDie` "hbs2-peer not found"

    let mt = vcat [ pretty k <> ":" <+> pretty v | (k,v) <- HM.toList meta ]
               & show & Text.pack

    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      let sto = AnyStorage (StorageClient caller)

      t0 <- writeAsMerkle sto lbs
              >>= getBlock sto
              >>= orThrowUser "can't read merkle tree just written"
              <&> deserialiseOrFail @(MTree [HashRef])
              >>= orThrowUser "merkle tree corrupted/invalid"

      -- FIXME: support-encryption
      let mann = MTreeAnn (ShortMetadata mt) NullEncryption t0

      putBlock sto (serialise mann)
        >>= orThrowUser "can't write tree"
        <&> HashRef


helpList :: MonadUnliftIO m => Maybe String -> RunM c m ()
helpList p = do

  let match = maybe (const True) (Text.isPrefixOf . Text.pack) p

  d <- ask >>= readTVarIO <&> fromDict
  let ks = [k | Id k <- List.sort (HM.keys d)
           , match k
           ]

  display_ $ vcat (fmap pretty ks)

main :: IO ()
main = do

  setupLogger

  cli <- getArgs <&> unlines . fmap unwords . splitForms
           >>= either (error.show) pure . parseTop

  let dict = execWriter do

        tell $ bindMatch "help" $ nil_ $ \syn -> do

            display_ $ "hbs2-cli tool" <> line

            case syn of
              (StringLike p : _) -> do
                helpList (Just p)

              [ListVal (SymbolVal "lambda" : SymbolVal what : _ )] -> do
                liftIO $ hPutDoc stdout $
                  "function" <+> ul (pretty what)
                  <> line

              _ -> helpList Nothing


        tell $ bindMatch "concat" $ \syn -> do

          case syn of
            [ListVal (StringLikeList xs)] -> do
              pure $ mkStr @C ( mconcat xs )

            StringLikeList xs -> do
              pure $ mkStr ( mconcat xs )

            _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "list" $ \case
          es -> do
            pure $ mkList @C es

        tell $ bindMatch "dict" $ \case
          es -> do
            pure $ mkForm "dict" es


        tell $ bindMatch "map" $ \syn -> do
          case syn of
            [ListVal (SymbolVal "lambda" : SymbolVal fn : _), ListVal rs] -> do
              mapM (apply fn . List.singleton) rs
                <&> mkList

            w -> do
              throwIO (BadFormException @C nil)

        tell $ bindMatch "head" $ \case
          [ ListVal es ] -> pure (head es)
          _ -> throwIO (TypeCheckError @C nil)

        tell $ bindMatch "tail" $ \case
          [] -> pure nil
          [ListVal []] -> pure nil
          [ListVal es] -> pure $ mkList @C (tail es)
          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "lookup" $ \case
          [s, ListVal (SymbolVal "dict" : es) ] -> do
            let val = headDef nil [ v | ListVal [k, v] <- es, k == s ]
            pure val

          [StringLike s, ListVal [] ] -> do
            pure nil

          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "display" $ nil_ \case
          [ sy ] -> display sy
          ss     -> display (mkList ss)

        tell $ bindMatch "debug:show-cli" $ nil_ \case
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

        tell $ bindMatch "hbs2:keyring:list-encryption" $ \syn -> do
          lbs <- case syn of

                [ ListVal [ SymbolVal "file", StringLike fn ] ] -> do
                  liftIO $ BS.readFile fn

                [ LitStrVal s ] -> do
                  pure (BS8.pack (Text.unpack s))

                _ -> throwIO (BadFormException @C nil)

          cred <- pure (parseCredentials @'HBS2Basic (AsCredFile lbs))
                    `orDie` "bad keyring file"

          let e = [ mkStr @C (show (pretty (AsBase58 p))) | KeyringEntry p _ _ <- view peerKeyring cred ]

          pure $ mkList @C e

        tell $ bindMatch "hbs2:keyring:new" $ \syn -> do
            n <- case syn of
                  [LitIntVal k] -> pure k
                  []            -> pure 1
                  _ -> throwIO (BadFormException @C nil)

            cred0 <- newCredentials @'HBS2Basic
            cred <- foldM (\cred _ -> addKeyPair Nothing cred) cred0 [1..n]
            pure $ mkStr @C $ show $ pretty $ AsCredFile $ AsBase58 cred

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

        tell $ bindMatch "str:read-stdin" $ \case
          [] -> liftIO getContents <&> mkStr @C

          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "str:read-file" $ \case
          [StringLike fn] -> liftIO (readFile fn) <&> mkStr @C

          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "str:save" $ nil_ \case
          [StringLike fn, StringLike what] ->
            liftIO (writeFile fn what)

          _ -> throwIO (BadFormException @C nil)


        tell $ bindMatch "hbs2:tree:metadata:get" $ \case
          [ SymbolVal how, StringLike hash ] -> do

            -- FIXME: put-to-the-state
            so <- detectRPC `orDie` "hbs2-peer not found"

            r <- withRPC2 @StorageAPI  @UNIX so $ \caller -> do
              let sto = AnyStorage (StorageClient caller)

              runMaybeT do

                headBlock <- getBlock sto (fromString hash)
                               >>= toMPlus
                               <&> deserialiseOrFail @(MTreeAnn [HashRef])
                               >>= toMPlus

                case headBlock of
                  MTreeAnn { _mtaMeta = ShortMetadata s } -> do
                    pure $ mkStr @C s

                  MTreeAnn { _mtaMeta = AnnHashRef h } -> do
                    getBlock sto h
                       >>= toMPlus
                       <&> LBS.toStrict
                       <&> TE.decodeUtf8
                       <&> mkStr @C

                  _ -> mzero


            case (how, r) of
              ("parsed", Just (LitStrVal r0)) -> do


                let xs = parseTop r0
                           & fromRight mempty

                pure $ mkForm @C "dict" xs

              _ -> pure $ fromMaybe nil r

          _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "hbs2:tree:metadata:create" $ \syn -> do

          case syn of

            (LitStrVal s : meta) -> do
              let lbs = fromString (Text.unpack s) :: LBS.ByteString
              h <- createTreeWithMetadata (metaFromSyntax meta) lbs
              pure $ mkStr (show $ pretty h)

            (ListVal [SymbolVal "from-file", StringLike fn ] : meta) -> do
              lbs <- liftIO $ LBS.readFile fn
              h <- createTreeWithMetadata (metaFromSyntax meta) lbs
              pure $ mkStr (show $ pretty h)

            (ListVal [SymbolVal "from-stdin"] : meta) -> do
              lbs <- liftIO $ LBS.getContents
              h <- createTreeWithMetadata (metaFromSyntax meta) lbs
              pure $ mkStr (show $ pretty h)

            _ -> throwIO (BadFormException @C nil)

        tell $ bindMatch "cbor:base58" $ \case
          [ LitStrVal x ] -> do
            pure $ mkForm "cbor:base58" [mkStr x]

          _ -> throwIO (BadFormException @C nil)


  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- getContents
                >>= either (error.show) pure . parseTop
      void $ run dict what

    [] -> do
      void $ run dict [mkForm  "help" []]

    _ -> do
      void $ run dict cli

