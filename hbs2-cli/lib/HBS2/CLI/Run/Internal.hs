{-# Language UndecidableInstances #-}
module HBS2.CLI.Run.Internal where

import HBS2.CLI.Prelude

import HBS2.OrDie
import HBS2.Base58
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.Kind
import Data.Maybe
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString (ByteString)
import Control.Monad.Identity
import Control.Monad.Writer

import Streaming.Prelude qualified as S

pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)

pattern BlobLike :: forall {c} . ByteString -> Syntax c
pattern BlobLike s <- (blobLike -> Just s)


class Display a where
  display :: MonadIO m => a -> m ()

instance  {-# OVERLAPPABLE #-} Pretty w => Display w  where
  display = liftIO . print . pretty

instance IsContext c => Display (Syntax c) where
  display = \case
    LitStrVal s -> liftIO $ TIO.putStr s
    ListVal [SymbolVal "small-encrypted-block", LitStrVal txt] -> do
      let s = Text.unpack txt & BS8.pack & toBase58 & AsBase58 & pretty
      liftIO $ print $ parens $ "small-encrypted-block" <+> parens ("blob" <+> dquotes s)
    ListVal [SymbolVal "blob", LitStrVal txt] -> do
      let s = Text.unpack txt & BS8.pack & toBase58 & AsBase58 & pretty
      liftIO $ print $ parens $ "blob:base58" <+> dquotes s

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

class IsContext c => MkInt c s where
  mkInt :: s -> Syntax c

instance (Integral i, IsContext c) => MkInt c i where
  mkInt n = Literal noContext $ LitInt (fromIntegral n)

class OptionalVal c b where
  optional :: b -> Syntax c -> b

instance IsContext c => OptionalVal c Int where
  optional d = \case
    LitIntVal x -> fromIntegral x
    _           -> d

hasKey :: IsContext c => Id -> [Syntax c] -> Maybe (Syntax c)
hasKey k ss = headMay [ e | ListVal [SymbolVal z, e] <- ss, z == k]

stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes

pattern Lambda :: forall {c}. [Syntax c] -> Syntax c -> Syntax c
pattern Lambda a e <- ListVal [SymbolVal "lambda", LambdaArgs a, e]

pattern LambdaArgs :: [Syntax c] -> Syntax c
pattern LambdaArgs a <- (lambdaArgList -> Just a)

lambdaArgList :: Syntax c -> Maybe [Syntax c]

lambdaArgList (ListVal a) = sequence argz
  where
    argz = flip fmap a \case
      x@(SymbolVal{}) -> Just x
      _               -> Nothing

lambdaArgList _ = Nothing

blobLike :: Syntax c -> Maybe ByteString
blobLike = \case
  LitStrVal s -> Just $ BS8.pack (Text.unpack s)
  ListVal [SymbolVal "blob", LitStrVal s] -> Just $ BS8.pack (Text.unpack s)
  _ -> Nothing

pattern PairList :: [Syntax c] -> [Syntax c]
pattern PairList es <- (pairList -> es)

pairList :: [Syntax c ] -> [Syntax c]
pairList syn = [ isPair s | s <- syn ] & takeWhile isJust & catMaybes

optlist :: IsContext c => [Syntax c] -> [(Id, Syntax c)]
optlist = reverse . go []
  where
    go acc ( SymbolVal i : b : rest ) = go ((i, b) : acc) rest
    go acc [ SymbolVal i ] = (i, nil) : acc
    go acc _ = acc


isPair :: Syntax c -> Maybe (Syntax c)
isPair = \case
  e@(ListVal [_,_]) -> Just e
  _ -> Nothing

data BindAction c ( m :: Type -> Type)  =
    BindLambda { fromLambda :: [Syntax c] -> RunM c m (Syntax c) }
  | BindValue  (Syntax c)

data Bind c ( m :: Type -> Type)  = Bind
  { bindAction      :: BindAction c m
  , bindName        :: Id
  , bindDescShort   :: Text
  } deriving (Generic)

deriving newtype instance Hashable Id

newtype NameNotBoundException =
  NameNotBound Id
  deriving stock Show
  deriving newtype (Generic,Typeable)

newtype NotLambda = NotLambda Id
                    deriving stock Show
                    deriving newtype (Generic,Typeable)

instance Exception NotLambda

data BadFormException c = BadFormException (Syntax c)
                        | ArgsMismatch (Syntax c)

newtype TypeCheckError c = TypeCheckError (Syntax c)

instance Exception (TypeCheckError C)

newtype BadValueException = BadValueException String
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

instance IsContext c => Show (BadFormException c) where
  show (BadFormException sy) = show $ "BadFormException" <+> pretty sy
  show (ArgsMismatch sy) = show $ "ArgsMismatch" <+> pretty sy

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



newtype MakeDictM c m a = MakeDictM { fromMakeDict :: Writer (Dict c m) a }
                          deriving newtype ( Applicative
                                           , Functor
                                           , Monad
                                           , MonadWriter (Dict c m)
                                           )

makeDict :: (IsContext c, Monad m) =>  MakeDictM c m () -> Dict c m
makeDict w = execWriter ( fromMakeDict w )

entry :: Dict c m  -> MakeDictM c m ()
entry = tell

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
  -- notice $ red "APPLY" <+> pretty name
  what <- ask >>=  readTVarIO <&> HM.lookup name . fromDict
  case bindAction <$> what of
    Just (BindLambda e) -> mapM runExpr args' >>= e
    Just (BindValue v)  -> throwIO (NotLambda name)
    Nothing -> throwIO (NameNotBound name)

runExpr :: forall c m . ( IsContext c
                        , MonadUnliftIO m
                        , Exception (BadFormException c)
                        ) => Syntax c -> RunM c m (Syntax c)
runExpr syn = handle (handleForm syn) $ do

    dict <- ask >>= readTVarIO <&> fromDict

    case syn of

      ListVal [ w, SymbolVal ".", b] -> do
        pure  $ mkList  [w, b]

      ListVal [ SymbolVal "quot", ListVal b] -> do
        pure  $ mkList  b

      ListVal [SymbolVal "lambda", arglist, body] -> do
        pure $ mkForm @c "lambda" [ arglist, body ]


      ListVal (SymbolVal "begin" : what) -> do
        evalTop what

      lc@(ListVal (Lambda decl body : args))  -> do

        when (length decl /= length args) do
          throwIO (ArgsMismatch lc)

        ev <- mapM runExpr args
        tv <- ask
        d0 <- readTVarIO tv
        atomically do
          forM_ (zip [ x | SymbolVal x <- decl ] ev) $ \(n,v) -> do
            let what = case v of
                        Lambda a b -> Bind (BindLambda (error "CALLIN FUCKING LAMBDA") ) "" ""
                        x -> Bind (BindValue x) "runtime-value" ""
            modifyTVar tv (Dict . HM.insert n what . fromDict)

        e <- runExpr body

        atomically $ writeTVar tv d0
        pure e

      ListVal (SymbolVal name : args') -> do
        apply name args'

      SymbolVal (Id s) | Text.isPrefixOf ":" s -> do
        pure (mkSym @c (Text.drop 1 s))

      SymbolVal name | HM.member name dict -> do
        let what = HM.lookup name dict
                     & maybe (BindValue (mkSym name)) bindAction

        case what of
          BindValue e  -> pure e
          BindLambda e -> pure $ mkForm "lambda" [mkSym name, mkSym "..."]

      e@(SymbolVal name) | not (HM.member name dict) -> do
        pure e

      e@Literal{} -> pure e

      e -> error (show $ "WTF:" <+> pretty e )

  where
    handleForm syn = \case
      (BadFormException _  :: BadFormException c) -> do
        throwIO (BadFormException syn)
      (ArgsMismatch s  :: BadFormException c) -> do
        throwIO (ArgsMismatch syn)

runM :: forall c m a. ( IsContext c
                      , MonadUnliftIO m
                      , Exception (BadFormException c)
                      ) => Dict c m -> RunM c m a ->  m a
runM  d  m = do
  tvd <- newTVarIO d
  runReaderT (fromRunM m) tvd

run :: forall c m . ( IsContext c
                    , MonadUnliftIO m
                    , Exception (BadFormException c)
                    ) => Dict c m -> [Syntax c] -> m (Syntax c)
run d sy = do
  tvd <- newTVarIO d
  lastDef nil <$> runReaderT (fromRunM (mapM runExpr sy)) tvd

evalTop :: forall c m . ( IsContext c
                        , MonadUnliftIO m
                        , Exception (BadFormException c))
     => [Syntax c]
     -> RunM c m (Syntax c)
evalTop syn = lastDef nil <$> mapM runExpr syn

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

internalEntries :: forall c m . (IsContext c, Exception (BadFormException c), MonadUnliftIO m) => MakeDictM c m ()
internalEntries = do
    entry $ bindMatch "concat" $ \syn -> do

      case syn of
        [ListVal (StringLikeList xs)] -> do
          pure $ mkStr ( mconcat xs )

        StringLikeList xs -> do
          pure $ mkStr ( mconcat xs )

        _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "list" $ \case
      es -> do
        pure $ mkList es

    entry $ bindMatch "dict" $ \case
      (pairList -> es@(_:_)) -> do
        pure $ mkForm "dict" es
      [a, b] -> do
        pure $ mkForm "dict" [ mkList [a, b] ]
      _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "kw" $ \syn -> do
      let wat = [ mkList @c [mkSym i, e] | (i,e) <- optlist syn ]
      pure $ mkForm "dict" wat

    entry $ bindMatch "lambda" $ \case
      [a, b] -> do
        pure $ mkForm  "lamba" [ mkSym "_", mkSym "..." ]

      _ -> error "SHIT"

    entry $ bindMatch "map" $ \syn -> do
      case syn of
        [ListVal (SymbolVal "lambda" : SymbolVal fn : _), ListVal rs] -> do
          mapM (apply @c fn . List.singleton) rs
            <&> mkList

        w -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "head" $ \case
      [ ListVal es ] -> pure (head es)
      _ -> throwIO (TypeCheckError @C nil)

    entry $ bindMatch "tail" $ \case
      [] -> pure nil
      [ListVal []] -> pure nil
      [ListVal es] -> pure $ mkList  (tail es)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "lookup" $ \case
      [s, ListVal (SymbolVal "dict" : es) ] -> do
        let val = headDef nil [ v | ListVal [k, v] <- es, k == s ]
        pure val

      [StringLike s, ListVal [] ] -> do
        pure nil

      _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "now" $ \case
      [] -> mkInt . round <$> liftIO getPOSIXTime
      _  -> throwIO (BadFormException @c nil)

    entry $ bindMatch "display" $ nil_ \case
      [ sy ] -> display sy
      ss     -> display (mkList ss)

    entry $ bindMatch "newline" $ nil_ $ \case
      [] -> liftIO (putStrLn "")
      _  -> throwIO (BadFormException @c nil)

    entry $ bindMatch "print" $ nil_ $ \case
      [ sy ] -> display sy
      ss     -> mapM_ display ss

    entry $ bindMatch "println" $ nil_ $ \case
      [ sy ] -> display sy >> liftIO (putStrLn "")
      ss     -> mapM_ display ss >> liftIO (putStrLn "")

    entry $ bindMatch "str:read-stdin" $ \case
      [] -> liftIO getContents <&> mkStr @c

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:put" $ nil_ $ \case
      [LitStrVal s] -> liftIO $ TIO.putStr s
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:read-file" $ \case
      [StringLike fn] -> liftIO (TIO.readFile fn) <&> mkStr

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:save" $ nil_ \case
      [StringLike fn, StringLike what] ->
        liftIO (writeFile fn what)

      _ -> throwIO (BadFormException @c nil)

    -- FIXME: we-need-opaque-type
    entry $ bindMatch "blob:read-stdin" $ \case
      [] -> do
        blob <- liftIO BS8.getContents <&> BS8.unpack
        pure (mkForm "blob" [mkStr @c blob])

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "blob:read-file" $ \case
      [StringLike fn] -> do
        blob <- liftIO (BS8.readFile fn) <&> BS8.unpack
        pure (mkForm "blob" [mkStr @c blob])

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "blob:save" $ nil_ $ \case
      [StringLike fn, ListVal [SymbolVal "blob", LitStrVal t]] -> do
        let s = Text.unpack t & BS8.pack
        liftIO $ BS8.writeFile fn s

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "blob:put" $ nil_ $ \case
      [ListVal [SymbolVal "blob", LitStrVal t]] -> do
        let s = Text.unpack t & BS8.pack
        liftIO $ BS8.putStr s

      _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "blob:base58" $ \case
      [LitStrVal t] -> do
        bs <- pure (Text.unpack t & BS8.pack & fromBase58)
               `orDie` "invalid base58"
              <&> BS8.unpack

        pure (mkForm "blob" [mkStr @c bs])

      _ -> throwIO (BadFormException @c nil)


    let decodeB58 t = do
          pure (Text.unpack t & BS8.pack & fromBase58)
            `orDie` "invalid base58"

    let decodeAndOut t = do
          liftIO $ BS8.putStr =<< decodeB58 t

    entry $ bindMatch "base58:encode" $ \case
      [LitStrVal t] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      [ListVal [SymbolVal "blob", LitStrVal t]] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:decode" $ \case

      [ListVal [SymbolVal "blob:base58", LitStrVal t]] -> do
        s <- decodeB58 t <&> BS8.unpack
        pure $ mkForm "blob" [mkStr @c s]

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:put" $ nil_ $ \case
      [ListVal [SymbolVal "blob:base58", LitStrVal t]] ->
        decodeAndOut t

      [LitStrVal t] -> decodeAndOut t

      e -> throwIO (BadFormException @c nil)

instance MonadUnliftIO m => HasStorage (RunM c m) where
  getStorage = do
    so <- detectRPC `orDie` "hbs2-peer not found"
    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      pure $ AnyStorage (StorageClient caller)

withPeerStorage :: (IsContext c, MonadUnliftIO m) => (AnyStorage -> RunM c m a) -> RunM c m a
withPeerStorage m = do
    so <- detectRPC `orDie` "hbs2-peer not found"

    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      let sto = AnyStorage (StorageClient caller)
      m sto


