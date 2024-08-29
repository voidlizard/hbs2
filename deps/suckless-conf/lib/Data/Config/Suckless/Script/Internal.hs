{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Data.Config.Suckless.Script.Internal
  ( module Data.Config.Suckless.Script.Internal
  , module Export
  ) where

import Data.Config.Suckless

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Data
import Data.Function as Export
import Data.Functor as Export
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.Maybe
import Data.String
import Data.Text.IO qualified as TIO
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX
import GHC.Generics hiding (C)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Safe
import Streaming.Prelude qualified as S
import System.Environment
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO

-- TODO: move-to-suckless-conf

data ManApplyArg = ManApplyArg Text Text
                   deriving stock (Eq,Show,Data,Generic)

newtype ManApply = ManApply [ ManApplyArg ]
                   deriving stock (Eq,Show,Data,Generic)
                   deriving newtype (Semigroup,Monoid)

data ManSynopsis =
  ManSynopsis ManApply
  deriving stock (Eq,Show,Data,Generic)

data ManDesc     = ManDescRaw Text
  deriving stock (Eq,Show,Data,Generic)

data ManRetVal   = ManRetVal
  deriving stock (Eq,Show,Data,Generic)

newtype ManName a = ManName Id
  deriving stock (Eq,Show,Data,Generic)
  deriving newtype (IsString,Pretty)

newtype ManBrief = ManBrief Text
  deriving stock (Eq,Show,Data,Generic)
  deriving newtype (Pretty,IsString)

data ManReturns = ManReturns Text Text
  deriving stock (Eq,Show,Data,Generic)

newtype ManExamples =
  ManExamples Text
  deriving stock (Eq,Show,Data,Generic)
  deriving newtype (Pretty,IsString,Monoid,Semigroup)

class ManNameOf a ann where
  manNameOf :: a -> ManName ann

data Man a =
  Man
  { manName     :: Maybe (ManName a)
  , manHidden   :: Bool
  , manBrief    :: Maybe ManBrief
  , manSynopsis :: [ManSynopsis]
  , manDesc     :: Maybe ManDesc
  , manReturns  :: Maybe ManReturns
  , manExamples :: [ManExamples]
  }
  deriving stock (Eq,Show,Generic)

instance Monoid (Man a) where
  mempty = Man Nothing False Nothing mempty Nothing Nothing mempty

instance Semigroup (Man a) where
  (<>) a b = Man (manName b <|> manName a)
                 (manHidden b || manHidden a)
                 (manBrief b <|> manBrief a)
                 (manSynopsis a <> manSynopsis b)
                 (manDesc b <|> manDesc a)
                 (manReturns b <|> manReturns a)
                 (manExamples a <> manExamples b)

instance ManNameOf Id a where
  manNameOf = ManName


instance Pretty ManDesc where
  pretty = \case
    ManDescRaw t -> pretty t

instance IsString ManDesc where
  fromString s = ManDescRaw (Text.pack s)

instance Pretty (Man a) where
  pretty e =   "NAME"
             <> line
             <> indent 8 (pretty (manName e) <> fmtBrief e)
             <> line
             <> fmtSynopsis
             <> fmtDescription
             <> retval
             <> fmtExamples
   where
    fmtBrief a = case manBrief a of
      Nothing -> mempty
      Just x  -> " - " <> pretty x

    retval = case manReturns e of
      Nothing -> mempty
      Just (ManReturns t s) ->
        line <> "RETURN VALUE" <> line
        <> indent 8 (
            if not (Text.null s) then
              (pretty t <> hsep ["","-",""] <> pretty s) <> line
            else pretty t )

    fmtDescription = line
      <> "DESCRIPTION" <> line
      <> indent 8 ( case manDesc e of
           Nothing -> pretty (manBrief e)
           Just x  -> pretty x)
      <> line

    fmtSynopsis = case manSynopsis e of
      [] -> mempty
      _  ->
           line
        <> "SYNOPSIS"
        <> line
        <> vcat (fmap synEntry (manSynopsis e))
        <> line

    fmtExamples = case manExamples e of
      [] -> mempty
      es -> line
        <> "EXAMPLES"
        <> line
        <> indent 8 ( vcat (fmap pretty es) )

    synEntry (ManSynopsis (ManApply [])) =
      indent 8 ( parens (pretty (manName e)) ) <> line

    synEntry (ManSynopsis (ManApply xs)) = do
      indent 8 do
        parens (pretty (manName e) <+>
         hsep [ pretty n | ManApplyArg t n <- xs ]  )
         <> line
         <> line
         <> vcat [ pretty n <+> ":" <+> pretty t | ManApplyArg t n <- xs ]

stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes

blobLike :: Syntax c -> Maybe ByteString
blobLike = \case
  LitStrVal s -> Just $ BS8.pack (Text.unpack s)
  ListVal [SymbolVal "blob", LitStrVal s] -> Just $ BS8.pack (Text.unpack s)
  _ -> Nothing

pattern BlobLike :: forall {c} . ByteString -> Syntax c
pattern BlobLike s <- (blobLike -> Just s)

class Display a where
  display :: MonadIO m => a -> m ()

instance  {-# OVERLAPPABLE #-} Pretty w => Display w  where
  display = liftIO . print . pretty

instance IsContext c => Display (Syntax c) where
  display = \case
    LitStrVal s -> liftIO $ TIO.putStr s
    -- ListVal [SymbolVal "small-encrypted-block", LitStrVal txt] -> do
    --   let s = Text.unpack txt & BS8.pack & toBase58 & AsBase58 & pretty
    --   liftIO $ print $ parens $ "small-encrypted-block" <+> parens ("blob" <+> dquotes s)
    -- ListVal [SymbolVal "blob", LitStrVal txt] -> do
    --   let s = Text.unpack txt & BS8.pack & toBase58 & AsBase58 & pretty
    --   liftIO $ print $ parens $ "blob:base58" <+> dquotes s
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

mkBool :: forall c . IsContext c => Bool -> Syntax c
mkBool v = Literal noContext (LitBool v)


class IsContext c => MkForm c a where
 mkForm :: a-> [Syntax c] -> Syntax c

instance (IsContext c, MkSym c s) => MkForm c s where
 mkForm s sy = List noContext ( mkSym @c s :  sy )

mkList :: forall c. IsContext c => [Syntax c] -> Syntax c
mkList = List noContext

isFalse :: forall c . IsContext c => Syntax c -> Bool
isFalse = \case
    Literal _ (LitBool False) -> True
    ListVal [] -> True
    _ -> False

eatNil :: Monad m => (Syntax c -> m a) -> Syntax c -> m ()
eatNil f = \case
  Nil -> pure ()
  x   -> void $ f x

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


pattern Lambda :: forall {c}. [Id] -> Syntax c -> Syntax c
pattern Lambda a e <- ListVal [SymbolVal "lambda", LambdaArgs a, e]

pattern LambdaArgs :: [Id] -> Syntax c
pattern LambdaArgs a <- (lambdaArgList -> Just a)


lambdaArgList :: Syntax c -> Maybe [Id]

lambdaArgList (ListVal a) = sequence argz
  where
    argz = flip fmap a \case
      (SymbolVal x) -> Just x
      _             -> Nothing

lambdaArgList _ = Nothing


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
  { bindMan         :: Maybe (Man AnsiStyle)
  , bindAction      :: BindAction c m
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
                        | ArityMismatch (Syntax c)

newtype TypeCheckError c = TypeCheckError (Syntax c)

instance Exception (TypeCheckError C)

newtype BadValueException = BadValueException String
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

instance IsContext c => Show (BadFormException c) where
  show (BadFormException sy) = show $ "BadFormException" <+> pretty sy
  show (ArityMismatch sy) = show $ "ArityMismatch" <+> pretty sy

instance IsContext c => Show (TypeCheckError c) where
  show (TypeCheckError sy) = show $ "TypeCheckError" <+> pretty sy

instance Exception (BadFormException C)

instance Exception BadValueException

type Dict c m = HashMap Id (Bind c m)

newtype RunM c m a = RunM { fromRunM :: ReaderT (TVar (Dict c m)) m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadReader (TVar (Dict c m))
                                      )

instance MonadTrans (RunM c) where
  lift = RunM . lift

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

hide :: MakeDictM c m ()
hide = pure ()

desc :: Doc ann -> MakeDictM c m () -> MakeDictM c m ()
desc txt = censor (HM.map setDesc)
  where
    w0 = mempty { manDesc = Just (ManDescRaw $ Text.pack $ show txt) }
    setDesc (Bind w x) = Bind (Just (maybe w0 (<> w0) w)) x

brief :: ManBrief -> MakeDictM c m () -> MakeDictM c m ()
brief txt = censor (HM.map setBrief)
  where
    w0 = mempty { manBrief = Just txt }
    setBrief (Bind w x) = Bind (Just (maybe w0 (<> w0) w)) x

returns :: Text -> Text -> MakeDictM c m () -> MakeDictM c m ()
returns tp txt = censor (HM.map setReturns)
  where
    w0 = mempty { manReturns = Just (ManReturns tp txt) }
    setReturns (Bind w x) = Bind (Just (maybe w0 (<>w0) w)) x


addSynopsis :: ManSynopsis -> Bind c m -> Bind c m
addSynopsis synopsis (Bind w x) = Bind (Just updatedMan) x
  where
    updatedMan = case w of
      Nothing -> mempty { manSynopsis = [synopsis] }
      Just man -> man { manSynopsis = manSynopsis man <> [synopsis] }

noArgs :: MakeDictM c m () -> MakeDictM c m ()
noArgs = censor (HM.map (addSynopsis (ManSynopsis (ManApply []))))

arg :: Text -> Text -> ManApplyArg
arg = ManApplyArg


args :: [ManApplyArg] -> MakeDictM c m () -> MakeDictM c m ()
args argList = censor (HM.map (addSynopsis (ManSynopsis (ManApply argList))))

opt :: Doc a -> Doc a -> Doc a
opt n d = n <+> "-" <+> d

examples :: ManExamples -> MakeDictM c m () -> MakeDictM c m ()
examples (ManExamples s) = censor (HM.map setExamples )
  where
    ex = ManExamples (Text.unlines $ Text.strip <$> Text.lines (Text.strip s))
    ex0 = mempty { manExamples = [ex] }
    setExamples (Bind w x) = Bind (Just (maybe ex0 (<>ex0) w)) x

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

applyLambda :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            )
            => [Id]
            -> Syntax c
            -> [Syntax c]
            -> RunM c m (Syntax c)
applyLambda decl body args = do

  when (length decl /= length args) do
    throwIO (ArityMismatch @c nil)

  ev <- mapM eval args
  tv <- ask
  d0 <- readTVarIO tv

  forM_ (zip decl ev) $ \(n,v) -> do
    bind n v

  e <- eval body

  atomically $ writeTVar tv d0
  pure e

apply_ :: forall c m . ( IsContext c
                      , MonadUnliftIO m
                      , Exception (BadFormException c)
                      )
      => Syntax c
      -> [Syntax c]
      -> RunM c m (Syntax c)

apply_ s args = case s of
  ListVal [SymbolVal "builtin:lambda", SymbolVal n]  -> apply n args
  SymbolVal   what  -> apply what args
  Lambda d body     -> applyLambda d body args
  e -> throwIO $ BadFormException @c s

apply :: forall c m . ( IsContext c
                      , MonadUnliftIO m
                      , Exception (BadFormException c)
                      )
      => Id
      -> [Syntax c]
      -> RunM c m (Syntax c)
apply name args' = do
  -- notice $ red "APPLY" <+> pretty name
  what <- ask >>=  readTVarIO <&> HM.lookup name

  case bindAction <$> what of
    Just (BindLambda e) -> mapM eval args' >>= e

    Just (BindValue (Lambda argz body) )  -> do
      applyLambda argz body args'

    Just (BindValue _) -> do
      throwIO (NotLambda name)

    Nothing -> throwIO (NameNotBound name)

bind :: forall c m . ( IsContext c
                     , MonadUnliftIO m
                     , Exception (BadFormException c)
                     )
     => Id
     -> Syntax c
     -> RunM c m ()
bind name expr = do
  t <- ask

  what <- case expr of
           ListVal [SymbolVal "builtin:lambda", SymbolVal n] -> do
             m <- readTVarIO t
             HM.lookup n m & maybe (throwIO (NameNotBound n)) pure

           e -> pure $ Bind mzero (BindValue e)

  atomically do
    modifyTVar t (HM.insert name what)

bindBuiltins ::  forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , Exception (BadFormException c)
                              )
            => Dict c m
            -> RunM c m ()

bindBuiltins  dict = do
  t <- ask
  atomically do
    modifyTVar t (<> dict)

eval :: forall c m . ( IsContext c
                     , MonadUnliftIO m
                     , Exception (BadFormException c)
                     ) => Syntax c -> RunM c m (Syntax c)
eval syn = handle (handleForm syn) $ do

    dict <- ask >>= readTVarIO

    case syn of

      ListVal [ w, SymbolVal ".", b] -> do
        pure  $ mkList  [w, b]

      ListVal [ SymbolVal "quot", ListVal b] -> do
        pure  $ mkList  b

      ListVal [SymbolVal "define", SymbolVal what, e] -> do
        ev <- eval e
        bind what ev>> pure nil

      ListVal [SymbolVal "lambda", arglist, body] -> do
        pure $ mkForm @c "lambda" [ arglist, body ]

      ListVal [SymbolVal "define", LambdaArgs (name : args), e] -> do
        bind name ( mkForm @c  "lambda" [ mkList [ mkSym s | s <- args], e ] )
        pure nil

      ListVal [SymbolVal "false?", e'] -> do
        e <- eval e'
        pure $ if isFalse e then mkBool True else mkBool False

      ListVal [SymbolVal "if", w, e1, e2] -> do
        what <- eval w
        if isFalse what then eval e2 else eval e1

      ListVal (SymbolVal "begin" : what) -> do
        evalTop what

      e@(ListVal (SymbolVal "blob" : what)) -> do
        pure e
        -- evalTop what

      lc@(ListVal (Lambda decl body : args))  -> do
        applyLambda decl body args

      ListVal (SymbolVal name : args') -> do
        apply name args'

      SymbolVal (Id s) | Text.isPrefixOf ":" s -> do
        pure (mkSym @c (Text.drop 1 s))

      SymbolVal name | HM.member name dict -> do
        let what = HM.lookup name dict
                     & maybe (BindValue (mkSym name)) bindAction

        case what of
          BindValue e  -> pure e
          BindLambda e -> pure $ mkForm "builtin:lambda" [mkSym name]

      e@(SymbolVal name) | not (HM.member name dict) -> do
        pure e

      e@Literal{} -> pure e

      e -> throwIO $ BadFormException @c e

  where
    handleForm syn = \case
      (BadFormException _  :: BadFormException c) -> do
        throwIO (BadFormException syn)
      (ArityMismatch s  :: BadFormException c) -> do
        throwIO (ArityMismatch syn)

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
  lastDef nil <$> runReaderT (fromRunM (mapM eval sy)) tvd

evalTop :: forall c m . ( IsContext c
                        , MonadUnliftIO m
                        , Exception (BadFormException c))
     => [Syntax c]
     -> RunM c m (Syntax c)
evalTop syn = lastDef nil <$> mapM eval syn

bindMatch :: Id -> ([Syntax c] -> RunM c m (Syntax c)) -> Dict c m
bindMatch n fn = HM.singleton n (Bind man (BindLambda fn))
  where
    man = Just $ mempty { manName = Just (manNameOf n) }

bindValue :: Id -> Syntax c -> Dict c m
bindValue n e = HM.singleton n (Bind mzero (BindValue e))

nil :: forall c . IsContext c => Syntax c
nil = List noContext []

nil_ :: (IsContext c, MonadIO m) =>  (a -> RunM c m b) -> a -> RunM c m (Syntax c)
nil_ m w = m w >> pure (List noContext [])

fixContext :: (IsContext c1, IsContext c2) => Syntax c1 -> Syntax c2
fixContext = go
  where
    go = \case
      List    _ xs -> List noContext (fmap go xs)
      Symbol  _ w  -> Symbol noContext w
      Literal _ l  -> Literal noContext l


fmt :: Syntax c -> Doc ann
fmt = \case
  LitStrVal x -> pretty $ Text.unpack x
  x -> pretty x

internalEntries :: forall c m . (IsContext c, Exception (BadFormException c), MonadUnliftIO m) => MakeDictM c m ()
internalEntries = do

    entry $ bindValue "false"      (mkBool False)
    entry $ bindValue "true"       (mkBool True)
    entry $ bindValue "chr:semi"   (mkStr ";")
    entry $ bindValue "chr:tilda"  (mkStr "~")
    entry $ bindValue "chr:colon"  (mkStr ":")
    entry $ bindValue "chr:comma"  (mkStr ",")
    entry $ bindValue "chr:q"      (mkStr "'")
    entry $ bindValue "chr:minus"  (mkStr "-")
    entry $ bindValue "chr:dq"     (mkStr "\"")
    entry $ bindValue "chr:lf"     (mkStr "\n")
    entry $ bindValue "chr:cr"     (mkStr "\r")
    entry $ bindValue "chr:tab"    (mkStr "\t")
    entry $ bindValue "chr:space"  (mkStr " ")

    brief "concatenates list of string-like elements into a string"
      $ args [arg "list" "(list ...)"]
      $ args [arg "..." "..."]
      $ returns "string" ""
      $ examples [qc|
        (concat a b c d)
        abcd|]
      $ examples [qc|
        (concat 1 2 3 4 5)
        12345|]

      $ entry $ bindMatch "concat" $ \syn -> do

        case syn of
          [ListVal xs] -> do
            pure $ mkStr ( show $ hcat (fmap fmt xs) )

          xs -> do
            pure $ mkStr ( show $ hcat (fmap fmt xs) )

    brief "creates a list of elements"
      $ args [arg "..." "..."]
      $ returns "list" ""
      $ examples [qc|
(list 1 2 3 fuu bar "baz")
(1 2 3 fuu bar "baz")
      |]
      $ entry $ bindMatch "list" $ \case
        es -> do
          pure $ mkList es

    entry $ bindMatch "dict" $ \case
      (pairList -> es@(_:_)) -> do
        pure $ mkForm "dict" es
      [a, b] -> do
        pure $ mkForm "dict" [ mkList [a, b] ]
      _ -> throwIO (BadFormException @C nil)

    brief "creates a dict from a linear list of string-like items"
      $ args [arg "list-of-terms" "..."]
      $ desc (    "macro; syntax sugar" <> line
               <> "useful for creating function args" <> line
               <> "leftover records are skipped"
             )
      $ returns "dict" ""
      $ examples [qc|
[kw a 1 b 2 c 3]
(dict (a 1) (b 2) (c 3))

[kw a]
(dict (a ()))

[kw a b]
(dict (a b))

[kw 1 2 3]
(dict)

[kw a b c]
(dict (a b) (c ()))
      |]
      $ entry $ bindMatch "kw" $ \syn -> do
         let wat = [ mkList @c [mkSym i, e] | (i,e) <- optlist syn ]
         pure $ mkForm "dict" wat

    entry $ bindMatch "iterate" $ nil_ $ \syn -> do
      case syn of
        [ListVal (SymbolVal "builtin:lambda" : SymbolVal fn : _), ListVal rs] -> do
          mapM_ (apply @c fn . List.singleton) rs

        [Lambda decl body,  ListVal args]  -> do
          mapM_ (applyLambda decl body . List.singleton) args

        _ -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "repeat" $ nil_ $ \case
      [LitIntVal n, Lambda [] b] -> do
        replicateM_ (fromIntegral n) (applyLambda [] b [])

      [LitIntVal n, e@(ListVal _)] -> do
        replicateM_ (fromIntegral n) (eval e)

      z ->
        throwIO (BadFormException @C nil)

    entry $ bindMatch "map" $ \syn -> do
      case syn of
        [ListVal (SymbolVal "builtin:lambda" : SymbolVal fn : _), ListVal rs] -> do
          mapM (apply @c fn . List.singleton) rs
            <&> mkList

        [Lambda decl body,  ListVal args]  -> do
          mapM (applyLambda decl body . List.singleton) args
            <&> mkList

        _ -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "head" $ \case
      [ ListVal es ] -> pure (head es)
      _ -> throwIO (TypeCheckError @C nil)

    brief "get tail of list"
      $ args [arg "list" "list"]
      $ desc "nil if the list is empty; error if not list"
      $ examples [qc|
      (tail [list 1 2 3])
      (2 3)
      (tail [list])
      |]
      $ entry $ bindMatch "tail" $ \case
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

    brief "returns current unix time"
      $ returns "int" "current unix time in seconds"
      $ noArgs
      $ entry $ bindMatch "now" $ \case
        [] -> mkInt . round <$> liftIO getPOSIXTime
        _  -> throwIO (BadFormException @c nil)

    entry $ bindMatch "display" $ nil_ \case
      [ sy ] -> display sy
      ss     -> display (mkList ss)

    brief "prints new line character to stdout"
      $ entry $ bindMatch "newline" $ nil_ $ \case
          [] -> liftIO (putStrLn "")
          _  -> throwIO (BadFormException @c nil)

    brief "prints a list of terms to stdout"
      $ entry $ bindMatch "print" $ nil_ $ \case
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

    brief "reads file as a string" do
      entry $ bindMatch "str:read-file" $ \case
        [StringLike fn] -> liftIO (TIO.readFile fn) <&> mkStr

        _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:save" $ nil_ \case
      [StringLike fn, StringLike what] ->
        liftIO (writeFile fn what)

      _ -> throwIO (BadFormException @c nil)

    entry $ bindValue "space" $ mkStr " "

    entry $ bindMatch "parse-top" $ \case

      [SymbolVal w, LitStrVal s] -> do
        pure $  parseTop s & either (const nil) (mkForm w . fmap fixContext)

      [LitStrVal s] -> do
        pure $  parseTop s & either (const nil) (mkList . fmap fixContext)

      _ -> throwIO (BadFormException @c nil)

    let atomFrom = \case
          [StringLike s] -> pure (mkSym s)
          [e]            -> pure (mkSym $ show $ pretty e)
          _ -> throwIO (BadFormException @c nil)

    brief "type of argument"
      $ args [arg "term" "term"]
      $ returns "symbol" "type"
      $ entry $ bindMatch "type" \case
        [ListVal _]    -> pure $ mkSym "list"
        [SymbolVal _]  -> pure $ mkSym "symbol"
        [LitStrVal _]  -> pure $ mkSym "string"
        [LitIntVal _]  -> pure $ mkSym "int"
        [LitScientificVal _] -> pure $ mkSym "float"
        [LitBoolVal _] -> pure $ mkSym "bool"
        _ -> throwIO (BadFormException @c nil)

    brief "creates a symbol from argument"
      $ args [arg "any-term" "term"]
      $ returns "symbol" ""
      do
        entry $ bindMatch "sym"  atomFrom
        entry $ bindMatch "atom" atomFrom

    brief "compares two terms" $
      args [arg "term" "a", arg "term" "b"] $
      returns "boolean" "#t if terms are equal, otherwise #f" $
        entry $ bindMatch "eq?" $ \case
          [a, b] -> do
            pure $ if a == b then mkBool True else mkBool False
          _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "length" $ \case
      [ListVal es] -> pure $ mkInt (length es)
      [StringLike es] -> pure $ mkInt (length es)
      _ -> pure $ mkInt 0

    entry $ bindMatch "nil?" $ \case
      [ListVal []] -> pure $ mkBool True
      _ -> pure $ mkBool False

    entry $ bindMatch "not" $ \case
      [w] -> do
        pure $ if isFalse w then mkBool True else mkBool False
      _ -> throwIO (BadFormException @c nil)

    brief "get system environment"
     $ args []
     $ args [ arg "string" "string" ]
     $ returns "env" "single var or dict of all vars"
     $ examples [qc|
      (env HOME)
      /home/user

      (env)
      (dict
         (HOME "/home/user") ... (CC "gcc") ...)
     |]
     $ entry $ bindMatch "env" $ \case
         [] -> do
           s <- liftIO getEnvironment
           pure $ mkForm "dict" [ mkList [mkSym @c a, mkStr b] | (a,b) <- s ]

         [StringLike s] -> do
           liftIO (lookupEnv s)
              <&> maybe nil mkStr
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


