{-# Language UndecidableInstances #-}
module HBS2.CLI.Run.Internal where

import HBS2.CLI.Prelude

import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.Kind
import Data.Maybe
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Control.Monad.Identity
import Control.Monad.Writer

import Streaming.Prelude qualified as S

pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)

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

newtype NameNotBoundException =
  NameNotBound Id
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
runExpr syn = handle (handleForm syn) $ case syn of

  ListVal [ w, SymbolVal ".", b] -> do
    pure  $ mkList  [w, b]

  ListVal [ SymbolVal "quot", ListVal b] -> do
    pure  $ mkList  b

  ListVal [SymbolVal "lambda", arglist, body] -> do
    pure $ mkForm @c "lambda" [ arglist, body ]

  ListVal (ListVal [SymbolVal "lambda", ListVal decl, body] : args)  -> do
    error "oopsie"
    -- d <- ask
    -- void $ liftIO do
    --   dd <- readTVarIO d
    --   undefined
      -- runReaderT $ runExpr body
    --   error "FUCK!"
    -- -- liftIO (run d body)
    pure nil

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

internalEntries :: MonadUnliftIO m => MakeDictM C m ()
internalEntries = do
    entry $ bindMatch "concat" $ \syn -> do

      case syn of
        [ListVal (StringLikeList xs)] -> do
          pure $ mkStr @C ( mconcat xs )

        StringLikeList xs -> do
          pure $ mkStr ( mconcat xs )

        _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "list" $ \case
      es -> do
        pure $ mkList @C es

    entry $ bindMatch "dict" $ \case
      es -> do
        pure $ mkForm "dict" es

    entry $ bindMatch "lambda" $ \case
      [a, b] -> do
        pure $ mkForm @C "lamba" [ mkSym "_", mkSym "..." ]

      _ -> error "SHIT"

    entry $ bindMatch "map" $ \syn -> do
      case syn of
        [ListVal (SymbolVal "lambda" : SymbolVal fn : _), ListVal rs] -> do
          mapM (apply fn . List.singleton) rs
            <&> mkList

        w -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "head" $ \case
      [ ListVal es ] -> pure (head es)
      _ -> throwIO (TypeCheckError @C nil)

    entry $ bindMatch "tail" $ \case
      [] -> pure nil
      [ListVal []] -> pure nil
      [ListVal es] -> pure $ mkList @C (tail es)
      _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "lookup" $ \case
      [s, ListVal (SymbolVal "dict" : es) ] -> do
        let val = headDef nil [ v | ListVal [k, v] <- es, k == s ]
        pure val

      [StringLike s, ListVal [] ] -> do
        pure nil

      _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "display" $ nil_ \case
      [ sy ] -> display sy
      ss     -> display (mkList ss)

    entry $ bindMatch "newline" $ nil_ $ \case
      [] -> liftIO (putStrLn "")
      _  -> throwIO (BadFormException @C nil)

    entry $ bindMatch "print" $ nil_ $ \case
      [ sy ] -> display sy >> liftIO (putStrLn "")
      ss     -> mapM_ display ss >> liftIO (putStrLn "")



