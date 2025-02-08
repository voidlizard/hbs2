{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language RecordWildCards #-}
{-# Language MultiWayIf #-}
module Data.Config.Suckless.Script.Internal
  ( module Data.Config.Suckless.Script.Internal
  , module Export
  ) where

import Data.Config.Suckless
import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse.Fuzzy as P
import Data.Config.Suckless.Almost.RPC
import Data.Config.Suckless.System

import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson as Aeson
import Data.Yaml qualified as Yaml
import Data.Ini qualified as Ini
import Data.Ini (Ini(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Data
import Data.Coerce
import Data.Function as Export
import Data.Functor as Export
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Kind
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.List ((\\))
import Data.Maybe
import Data.Either
import Data.String
import Data.Text.IO qualified as TIO
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With,encodeUtf8)
import Data.Text.Encoding.Error (ignore)
import Data.Time.Clock.POSIX
import Data.Time.Format (defaultTimeLocale, formatTime)

import HTMLEntities.Text as Html
import GHC.Generics hiding (C)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Safe
import Streaming.Prelude qualified as S
import System.Environment
import System.Directory qualified as Dir
import System.FilePath.Posix as P
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO
import Control.Monad.Trans.Cont

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
             <> indent 4 (pretty (manName e) <> fmtBrief e)
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
        <> indent 4 (
            if not (Text.null s) then
              (pretty t <> hsep ["","-",""] <> pretty s) <> line
            else pretty t )

    fmtDescription = line
      <> "DESCRIPTION" <> line
      <> indent 4 ( case manDesc e of
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
        <> indent 4 ( vcat (fmap pretty es) )

    synEntry (ManSynopsis (ManApply [])) =
      indent 4 ( parens (pretty (manName e)) ) <> line

    synEntry (ManSynopsis (ManApply xs)) = do
      indent 4 do
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

toSortable :: Syntax c -> Either Double Text
toSortable = \case
  LitIntVal n         -> Left (fromIntegral n)
  LitScientificVal n  -> Left (realToFrac n)
  LitBoolVal False    -> Left 0
  LitBoolVal True     -> Left 1
  LitStrVal s         -> Right s
  SymbolVal (Id s)    -> Right s
  ListVal es          -> Left (fromIntegral (length es))
  OpaqueValue box     -> Left 0
  _                   -> Left 0

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


isFalse :: forall c . IsContext c => Syntax c -> Bool
isFalse = \case
    Literal _ (LitBool False) -> True
    ListVal [] -> True
    _ -> False

isTrue :: forall c . IsContext c => Syntax c -> Bool
isTrue = not . isFalse

eatNil :: Monad m => (Syntax c -> m a) -> Syntax c -> m ()
eatNil f = \case
  Nil -> pure ()
  x   -> void $ f x

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

-- FIXME: detect-invalid-varags
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
    go acc ( TextLike i : b : rest ) = go ((Id i, b) : acc) rest
    go acc [ TextLike i ] = (Id i, nil) : acc
    go acc _ = acc


isPair :: Syntax c -> Maybe (Syntax c)
isPair = \case
  e@(ListVal [_,_]) -> Just e
  _ -> Nothing

data BindAction c ( m :: Type -> Type)  =
    BindLambda { fromLambda :: [Syntax c] -> RunM c m (Syntax c) }
  | BindMacro  { fromMacro  :: [Syntax c] -> RunM c m (Syntax c) }
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


data BadFormException c = BadFormException (Syntax c)
                        | ArityMismatch (Syntax c)
                        | NotLambda (Syntax c)
                        | NotBuiltinLambda Id
                        | RuntimeError (Syntax c)
                        | TypeCheckError (Syntax c)

newtype BadValueException = BadValueException String
                            deriving stock Show
                            deriving newtype (Generic,Typeable)

instance Exception NameNotBoundException

instance IsContext c => Show (BadFormException c) where
  show (BadFormException sy) = show $ "BadFormException" <+> pretty sy
  show (ArityMismatch sy) = show $ "ArityMismatch" <+> pretty sy
  show (NotLambda sy) = show $ "NotLambda" <+> pretty sy
  show (TypeCheckError sy) = show $ "TypeCheckError" <+> pretty sy
  show (RuntimeError sy) = show $ "RuntimeError" <+> pretty sy
  show (NotBuiltinLambda sy) = show $ "NotBuiltinLambda" <+> pretty sy

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

hide ::  Bind c m -> Bind c m
hide (Bind w x) = Bind (Just updatedMan) x
  where
    updatedMan = case w of
      Nothing -> mempty { manHidden = True }
      Just man -> man { manHidden = True }

hidden :: MakeDictM c m () -> MakeDictM c m ()
hidden = censor (HM.map hide)

hidePrefix :: Id -> MakeDictM c m () -> MakeDictM c m ()
hidePrefix (Id p) = error "hidePrefix does not work yet"
  -- censor (HM.filterWithKey exclude)
  where
    exclude (Id k) _ = not (Text.isPrefixOf p k)

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
    ex = ManExamples (Text.unlines $ Text.lines (Text.strip s))
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



evargs :: forall c m . ( IsContext c
                       , MonadUnliftIO m
                       , Exception (BadFormException c)
                       )
       => Dict c m
       -> [Syntax c]
       -> RunM c m [Syntax c]

evargs dict = mapM (eval' dict)

applyLambda :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            )
            => [Id]
            -> Syntax c
            -> [Syntax c]
            -> RunM c m (Syntax c)
applyLambda decl body ev = do

  let (manda,opt) = List.break (== ".") decl

  when (length manda > length ev) do
    throwIO (ArityMismatch @c nil)

  tv <- ask
  d0 <- readTVarIO tv

  let (mandatory,optional) = splitAt (length manda) ev

  forM_ (zip decl mandatory) $ \(n,v) -> do
    bind n v

  forM_ (headMay (tailSafe opt)) $ \n -> do
    bind n (mkList optional)

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
  SymbolVal "quasiquot"   -> mkList <$> mapM (evalQQ mempty) args
  SymbolVal "quasiquote"  -> mkList <$> mapM (evalQQ mempty) args
  SymbolVal   what  -> apply what args
  Lambda d body     -> applyLambda d body args
  e                 -> throwIO $ NotLambda e

apply :: forall c m . ( IsContext c
                      , MonadUnliftIO m
                      , Exception (BadFormException c)
                      )
      => Id
      -> [Syntax c]
      -> RunM c m (Syntax c)

apply "quot" e = case e of
  [ x ] -> pure x
  _ -> throwIO $ BadFormException @c nil

apply "quasiquot" args = do
  mkList <$> mapM (evalQQ mempty) args

apply name args' = do
  what <- ask >>=  readTVarIO <&> HM.lookup name

  case bindAction <$> what of
    Just (BindLambda e) -> do
      e args'

    Just (BindValue (Lambda argz body) )  -> do
      applyLambda argz body args'

    Just (BindMacro macro) -> do
      macro args'

    Just (BindValue _) -> do
      throwIO (NotLambda (mkSym @c name))

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


evalQQ :: forall c m . ( IsContext c
                     , MonadUnliftIO m
                     , Exception (BadFormException c)
                     ) => Dict c m
                       -> Syntax c -> RunM c m (Syntax c)
evalQQ d0 = \case
  -- SymbolVal (Id w) | Text.isPrefixOf "," w -> do
  --   let what = Id (Text.drop 1 w)
  --   lookupValue what >>= eval

  ListVal [ SymbolVal ",", w ] -> eval' d0 w

  List c es   -> List c <$> mapM (evalQQ d0) es

  other       -> pure other

eval :: forall c m . ( IsContext c
                     , MonadUnliftIO m
                     , Exception (BadFormException c)
                     )
                       => Syntax c
                       -> RunM c m (Syntax c)
eval = eval' mempty


eval' :: forall c m . ( IsContext c
                     , MonadUnliftIO m
                     , Exception (BadFormException c)
                     ) => Dict c m
                       -> Syntax c
                       -> RunM c m (Syntax c)
eval' dict0 syn' = handle (handleForm syn') $ do

    dict1 <- ask >>= readTVarIO

    let dict = dict0 <> dict1

    -- liiftIO $ print $ show $ "TRACE EXP" <+> pretty syn
    let importDecls = HS.fromList [ "import", "define", "define-macro" :: Id ]

    let isDefine x = x == "define" || x == "local"

    case syn' of

      SymbolVal (Id s) | Text.isPrefixOf ":" s -> do
        pure (mkSym @c (Text.drop 1 s))

      ListVal [ w, SymbolVal ".", b] -> do
        pure  $ mkList  [w, b]

      ListVal [ SymbolVal ":", b] -> do
        pure  $ mkList [b]

      ListVal [ SymbolVal "'", ListVal b] -> do
        pure  $ mkList b

      ListVal [ SymbolVal "'", StringLike x] -> do
        pure  $ mkSym x

      ListVal [ SymbolVal "'", x] -> do
        pure  x

      ListVal [ SymbolVal ",", x] -> do
        pure  x

      ListVal [ SymbolVal "`", ListVal b] -> do
        mkList <$> mapM (evalQQ dict) b

      ListVal [ SymbolVal "quasiquot", ListVal b] -> do
        mkList <$> mapM (evalQQ dict) b

      ListVal [ SymbolVal "quot", b] -> do
        pure b

      ListVal [ SymbolVal "eval", e ] -> eval e >>= eval

      ListVal [ SymbolVal "import", StringLike fn ] -> do

        let importsName = "*runtime-imports*"
        let alreadyError = RuntimeError $ mkForm "runtime-error" [ mkStr @c ["already imported", pretty fn] ]
        let disappearedMessage = [mkStr @c [coerce importsName, "misteriously disappeared" :: Text]]
        let disappeared = RuntimeError $ mkForm "runtime-error" disappearedMessage

        initial <- newTVarIO (mempty :: HashMap Id (HashSet Id)) >>= mkOpaque

        imp_ <- lookupValueDef initial importsName >>= \case
                   OpaqueVal e -> fromOpaque @(TVar (HashMap Id (HashSet Id))) e & \case
                                    Just x  -> pure x
                                    Nothing -> throwIO disappeared

                   _           -> throwIO (RuntimeError (mkStr @c $ show $ pretty importsName <> "misteriously disappeared"))


        seen <- atomically $ stateTVar imp_ (\e -> (HM.lookup (mkId fn) e, HM.insert (mkId fn) mempty e))

        -- liftIO $ print $ pretty "import" <+> pretty fn

        -- TODO: maybe-should-be-error
        case seen of
          Just{} -> pure nil
          Nothing{} -> do

            -- FIXME: fancy-error-handling
            syn <- liftIO (TIO.readFile fn) <&> parseTop >>= either(error.show) pure

            let decls = [ fixContext d
                        | d@(ListVal (SymbolVal what : rest)) <- syn
                        , what `HS.member` importDecls
                        ]

            void $ evalTop decls

            pure nil

      ListVal [SymbolVal def, SymbolVal what, e] | isDefine def -> do
        ev <- eval e
        bind what ev>> pure nil

      ListVal [SymbolVal "define-macro",  LambdaArgs (name:argz), e] -> do
        t <- ask

        let runMacro argvalz = do
             de <- forM (zip argz argvalz) $ \(n,e) -> do
                     v <- eval e
                     pure (n, Bind mzero (BindValue v))

             let d0 = HM.fromList de

             eval' d0 e >>= eval' d0

        let b = Bind mzero (BindMacro runMacro)
        atomically $ modifyTVar t (HM.insert name b)
        pure nil

      ListVal [SymbolVal "fn", LitIntVal n, body] -> do
        pure $ mkForm @c "lambda" [ mkList [ mkSym ("_" <> show i) | i <- [1..n]  ], body ]

      ListVal [SymbolVal "lambda", arglist, body] -> do
        pure $ mkForm @c "lambda" [ arglist, body ]

      ListVal [SymbolVal def, LambdaArgs (name : args), e] | isDefine def -> do
        bind name ( mkForm @c  "lambda" [ mkList [ mkSym s | s <- args], e ] )
        pure nil

      ListVal [SymbolVal "false?", e'] -> do
        e <- eval e'
        pure $ if isFalse e then mkBool True else mkBool False


      ListVal [SymbolVal "if", w, e] -> do
        what <- eval w
        if not (isFalse what) then eval e else pure nil

      ListVal [SymbolVal "if", w, e1, e2] -> do
        what <- eval w
        if isFalse what then eval e2 else eval e1

      ListVal [SymbolVal "unless", w, e1] -> do
        what <- eval w
        if isFalse what then eval e1 else pure nil

      ListVal (SymbolVal "begin" : what) -> do
        evalTop what

      e@(ListVal (SymbolVal "blob" : what)) -> do
        pure e
        -- evalTop what

      lc@(ListVal (Lambda decl body : args))  -> do
        applyLambda decl body =<< evargs dict args

      ListVal (SymbolVal name : args') -> do
        apply name =<< evargs dict args'

      ListVal (e' : args') -> do
        e <- eval e'
        apply_ e =<< evargs dict args'

      SymbolVal name | HM.member name dict -> do


        let what = HM.lookup name dict0 <|> HM.lookup name dict1
                     & maybe (BindValue (mkSym name)) bindAction

        -- liftIO $ print $ "LOOKUP" <+> pretty name <+> pretty what

        case what of
          BindValue e  -> pure e
          BindLambda e -> pure $ mkForm "builtin:lambda" [mkSym name]
          BindMacro _  -> pure nil

      e@(SymbolVal name) | not (HM.member name dict) -> do
        pure e

      e@Literal{} -> pure e

      e@OpaqueValue{} -> pure e

      e -> do
        throwIO $ NotLambda @c e

  where
    handleForm syn = \case
      (BadFormException _  :: BadFormException c) -> do
        throwIO (BadFormException syn)
      (ArityMismatch s  :: BadFormException c) -> do
        throwIO (ArityMismatch syn)
      (TypeCheckError s  :: BadFormException c) -> do
        throwIO (TypeCheckError syn)
      other -> throwIO other

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

runEval :: forall c m . ( IsContext c
                    , MonadUnliftIO m
                    , Exception (BadFormException c)
                    ) => TVar (Dict c m) -> [Syntax c] -> m (Syntax c)
runEval tvd sy = do
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

{- HLINT ignore "Redundant <&>" -}

bindAlias :: forall c m . ( MonadUnliftIO m
                          , IsContext c
                          , Exception (BadFormException c))
          => Id -> Id  -> Dict c m
bindAlias n fn = HM.singleton n (Bind man (BindLambda callAlias))
  where
    man = Just $ mempty { manName = Just (manNameOf n) }
    callAlias syn = do
      ask  >>= readTVarIO
           <&> (fmap bindAction . HM.lookup fn)
           >>= \case
              Just (BindLambda la) -> la syn
              _  -> throwIO (NotBuiltinLambda @c fn)

bindMacro :: Id -> ([Syntax c] -> RunM c m (Syntax c)) -> Dict c m
bindMacro n fn = HM.singleton n (Bind man (BindMacro fn))
  where
    man = Just $ mempty { manName = Just (manNameOf n) }

bindValue :: Id -> Syntax c -> Dict c m
bindValue n e = HM.singleton n (Bind mzero (BindValue e))

lookupValue :: forall c m . (IsContext c, MonadUnliftIO m)
            => Id -> RunM c m (Syntax c)
lookupValue i = do
  ask  >>= readTVarIO
       <&> (fmap bindAction . HM.lookup i)
       >>= \case
          Just (BindValue s) -> pure s
          _  -> throwIO (NameNotBound i)

lookupValueDef :: forall c m . (IsContext c, Exception (BadFormException c), MonadUnliftIO m)
               => Syntax c
               -> Id
               -> RunM c m (Syntax c)
lookupValueDef defVal i = do
  ask  >>= readTVarIO
       <&> (fmap bindAction . HM.lookup i)
       >>= \case
          Just (BindValue s) -> pure s
          _  -> do
            bind i defVal
            pure defVal

nil_ :: (IsContext c, MonadIO m) =>  (a -> RunM c m b) -> a -> RunM c m (Syntax c)
nil_ m w = m w >> pure (List noContext [])

fixContext :: forall c1 c2 . (IsContext c1, IsContext c2) => Syntax c1 -> Syntax c2
fixContext = go
  where
    go = \case
      List    _ xs -> List noContext (fmap go xs)
      Symbol  _ w  -> Symbol noContext w
      Literal _ l  -> Literal noContext l
      OpaqueValue box  -> OpaqueValue box

fixList :: forall c . IsContext c => Syntax c -> Syntax c
fixList = \case
  (ListVal es) -> mkList ( mkSym "list" : es )
  e -> e

fmt :: Syntax c -> Doc ann
fmt = \case
  LitStrVal x -> pretty $ Text.unpack x
  x -> pretty x

newtype IniConfig = IniConfig Ini.Ini

instance IsContext c => MkSyntax c IniConfig where
  mkSyntax (IniConfig  (Ini{..})) = do

    let section kvs = [ mkList [mkSym k, either (const (mkStr v)) fixContext (P.parseSyntax v)]
                      | (k,v) <- kvs
                      ]

    let globals = section iniGlobals

    let sections = [ mkForm @c s (section pps) | (s, pps)  <- HM.toList iniSections ]

    mkList (globals <> sections)

internalEntries :: forall c m . ( IsContext c
                                , Exception (BadFormException c)
                                , MonadUnliftIO m) => MakeDictM c m ()
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

    entry $ bindAlias "local" "define"

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

      $ entry $ bindMatch "concat" (pure . mkStr . foldMap synToText)

    let mkJoin x es = do
         let xs = List.intersperse x es
         pure $ mkStr ( show $ hcat (fmap fmt xs) )

    entry $ bindMatch "join" $ \case
      [ x, ListVal es ] -> mkJoin x es
      (x : es ) -> mkJoin x es
      _ -> throwIO (BadFormException @C nil)

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
        pure $ mkList es
      [a, b] -> do
        pure $ mkList [ mkList [a, b] ]
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
         let wat = mkList [ mkList @c [mkSym i, e] | (i,e) <- optlist syn ]
         pure $ wat

    entry $ bindMatch "iterate" $ nil_ $ \case
      [ what, ListVal es ] -> do
        mapM_ (apply_ what . List.singleton) es

      _ -> do
        throwIO (BadFormException @C nil)


    entry $ bindMatch "replicate" $ \case
      [LitIntVal n, e] -> pure $ mkList (replicate (fromIntegral n) e)
      _ -> pure nil

    entry $ bindMatch "repeat" $ nil_ $ \case
      [LitIntVal n, Lambda [] b] -> do
        replicateM_ (fromIntegral n) (applyLambda [] b [])

      [LitIntVal n, e@(ListVal _)] -> do
        replicateM_ (fromIntegral n) (eval e)

      z ->
        throwIO (BadFormException @C nil)

    entry $ bindMatch "bound?" $ \case
      [ SymbolVal x ] -> do
        error "DONT KNOW"
      _ -> pure $ mkBool False


    entry $ bindMatch "eval" $ \syn -> do
      r <- mapM eval syn
      pure $ lastDef nil r

    entry $ bindMatch "id" $ \case
      [ e ] -> pure e
      _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "true?" $ \case
      [ e ] | e == mkBool True -> pure $ mkBool True
      _ -> pure $ mkBool False

    entry $ bindMatch "inc" $ \case
      [ LitIntVal n ] -> pure (mkInt (succ n))
      _ -> throwIO (TypeCheckError @C  nil)

    entry $ bindMatch "dec" $ \case
      [ LitIntVal n ] -> pure (mkInt (pred n))
      _ -> throwIO (TypeCheckError @C  nil)

    entry $ bindMatch "map" $ \case
      [ what, ListVal es ] -> do
        mkList <$> mapM (apply_ what . List.singleton) es

      _ -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "quot" $ \case
      [ syn ] -> pure $ mkList [syn]
      _ -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "quasiquot" $ \case
      [ syn ] -> mkList . List.singleton <$> evalQQ mempty syn
      _ -> do
          throwIO (BadFormException @C nil)

    entry $ bindMatch "last" $ \case
      [ ListVal es ] -> pure (lastDef nil es)
      [ StringLike es ] -> pure $ maybe nil (mkSym . List.singleton) (lastMay es)
      _ -> throwIO (TypeCheckError @C nil)

    entry $ bindMatch "head" $ \case
      [ ListVal es ] -> pure (headDef nil es)
      [ StringLike es ] -> pure $ maybe nil (mkSym . List.singleton) (headMay es)
      _ -> throwIO (TypeCheckError @C nil)

    entry $ bindAlias "car" "head"

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

    entry $ bindAlias "cdr" "tail"

    entry $ bindMatch "cons" $ \case
      [ e, ListVal es ] -> pure (mkList (e:es))
      _ -> throwIO (BadFormException @C nil)

    entry $ bindMatch "@" $ \syn -> do
      case List.uncons (reverse syn) of
        Nothing -> pure nil
        Just (a, []) -> pure a
        Just (a, fs) -> flip fix (a, fs) $ \next -> \case
          (acc, []) -> pure acc
          (acc, x:xs) -> do
            acc' <- apply_ x [acc]
            next (acc', xs)


    entry $ bindMatch "split" $ \case
      [TextLike sep, TextLike s] ->
        pure $ mkList [mkStr x | x <- Text.splitOn sep s]

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "filter" $ \case
      [pred, ListVal xs] -> do
        filtered <- flip filterM xs $ \x -> do
          res <- apply_ pred [x]
          case res of
            LitBoolVal True -> pure True
            _ -> pure False

        pure $ mkList filtered

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "group-by" $ \case
      [cmp, ListVal es] -> do
        let groupByM _ [] = pure []
            groupByM eq (x:xs) = do
              (same, rest) <- partitionM (eq x) xs
              groups <- groupByM eq rest
              pure ((x:same) : groups)

        let eqFunc a b = do
              result <- apply_ cmp [a, b]
              pure $ case result of
                LitBoolVal v -> v
                _ -> False  -- Если не bool, считаем, что не равны

        grouped <- groupByM eqFunc es
        pure $ mkList [mkList group | group <- grouped]

      _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "sort-with" $ \case
      [cmp, ListVal es] -> do
        let cmpFunc a b = do
              result <- apply_ cmp [a, b]
              pure $ case result of
                LitBoolVal v -> v
                _ -> False  -- Если не bool, считаем `x < y` ложным

        sorted <- sortByM cmpFunc es
        pure $ mkList sorted

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "sort" $ \case
      [ListVal es] -> pure $ mkList $ (List.sortOn toSortable) es
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "sort-by" $ \case
      [what, ListVal es] -> do
        sorted <- forM es \e -> do
          key <- apply_ what [e]
          pure (key, e)

        pure $ mkList [e | (_, e) <- List.sortOn (toSortable . fst) sorted]

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "append" $ \syn -> do
      pure $ mkList $  flip fix (mempty, syn) $ \next (acc, terms) -> do
        case terms of
          [] -> acc
          (ListVal xs : rest) -> next (acc <> xs, rest)
          (other : rest) -> next (acc <> [other], rest)

    entry $ bindMatch "flatten" $ \case
      [ListVal es] -> pure $ mkList (concatMap flattenList es)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "reverse" $ \case
      [ListVal es] -> pure $ mkList (List.reverse es)
      [LitStrVal s] -> pure $ mkStr (Text.reverse s)
      [SymbolVal (Id s)] -> pure $ mkSym (Text.reverse s)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "nub" $ \case
      [ ListVal es ] -> pure $ mkList $ List.nub es
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "zip" $ \case
      [ ListVal a, ListVal b ] -> pure $ mkList (zipWith (\x y -> mkList [x,y]) a b)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "take" $ \case
      [ LitIntVal n, ListVal es ] -> pure $ mkList $ take (fromIntegral n) es
      [ LitIntVal n, StringLike es ] -> pure $ mkStr $ take (fromIntegral n) es
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "drop" $ \case
      [ LitIntVal n, ListVal es ] -> pure $ mkList $ drop (fromIntegral n) es
      [ LitIntVal n, StringLike es ] -> pure $ mkStr $ drop (fromIntegral n) es
      _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "nth" $ \case
      [LitIntVal i, ListVal es] -> do
        let idx = if i < 0 then length es + fromIntegral i else fromIntegral i
        pure $ atDef nil es idx

      [LitIntVal i, StringLike es] -> do
        let idx = if i < 0 then length es + fromIntegral i else fromIntegral i
        pure $ maybe nil (mkSym . List.singleton) $ atMay es idx

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "assoc" $ \case
      [k, ListVal es ] -> pure $ headDef nil [ r | r@(ListVal (w:_)) <- es, k == w ]
      _ -> throwIO (BadFormException @c nil)

    --TODO: integral sum

    entry $ bindMatch "upper" $ \case
      [ LitStrVal x ] -> pure $ mkStr $ Text.toUpper x
      [ SymbolVal (Id x) ] -> pure $ mkStr $ Text.toUpper x
      _ -> pure nil

    entry $ bindMatch "lower" $ \case
      [ LitStrVal x ] -> pure $ mkStr $ Text.toLower x
      [ SymbolVal (Id x) ] -> pure $ mkStr  $ Text.toLower x
      _ -> pure nil

    entry $ bindMatch "words" $ \case
      [ TextLike x ] -> pure $ mkList [ mkStr  y | y <- Text.words x ]
      _ -> pure nil

    entry $ bindMatch "unwords" $ \case
      [ ListVal (TextLikeList xs) ] -> pure $ mkStr (Text.unwords xs)
      ( TextLikeList xs) -> pure $ mkStr (Text.unwords xs)
      _ -> pure $ mkStr ""

    entry $ bindMatch "lines" $ \case
      [ TextLike x ] -> pure $ mkList [ mkStr  y | y <- Text.lines x ]
      _ -> pure nil

    entry $ bindMatch "mod" $ \case
      [LitIntVal a, LitIntVal b] | b /= 0 -> pure $ mkInt (a `mod` b)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindAlias "%" "mod"

    entry $ bindMatch "sum" $ \case
      [ ListVal es ]  -> do
        let v = flip mapMaybe es \case
                  LitIntVal n         -> Just $ realToFrac  n
                  LitScientificVal n  -> Just $ realToFrac @_ @Double n
                  _ -> Nothing

        pure $ mkDouble $ sum v

      _ -> pure $ mkDouble 0

    entry $ bindMatch "assoc:nth" $ \case
      [LitIntVal i, k, ListVal es ] -> do
        pure $ headDef nil [ r | r@(ListVal ys) <- es, atMay  ys (fromIntegral i) == Just k ]
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "lookup" $ \case
      [k, ListVal es ] -> do
        let val = headDef nil [ mkList rest | ListVal (w:rest) <- es, k == w ]
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

    let colorz = HM.fromList
          [ ("red",     pure (Red, True))
          , ("red~",    pure (Red, False))
          , ("green",   pure (Green, True))
          , ("green~",  pure (Green, False))
          , ("yellow",  pure (Yellow, True))
          , ("yellow~", pure (Yellow, False))
          , ("blue",    pure (Blue, True))
          , ("blue~",   pure (Blue, False))
          , ("magenta", pure (Magenta, True))
          , ("magenta~",pure (Magenta, False))
          , ("cyan",    pure (Cyan, True))
          , ("cyan~",   pure (Cyan, False))
          , ("white",   pure (White, True))
          , ("white~",  pure (White, False))
          , ("black",   pure (Black, True))
          , ("black~",  pure (Black, False))
          , ("_",       mzero)
          ]


    let fgc fg = case join (HM.lookup fg colorz) of
          Just (co, True)  -> color co
          Just (co, False) -> colorDull co
          Nothing             -> mempty

    let niceTerm f = \case
          LitStrVal x -> do
            let s = renderStrict $ layoutPretty defaultLayoutOptions (annotate f $ pretty x)
            mkStr s

          other -> do
            let s = renderStrict $ layoutPretty defaultLayoutOptions (annotate f $ pretty other)
            mkStr s

    entry $ bindMatch "ansi" $  \case
      [ SymbolVal fg, SymbolVal bg, term ] | HM.member fg colorz && HM.member bg colorz -> do
        let b = case join (HM.lookup bg colorz) of
                  Just (co, True)  -> bgColor co
                  Just (co, False) -> bgColorDull co
                  Nothing -> mempty

        let f = b <> fgc fg
        pure $ niceTerm f term

      [ SymbolVal fg, s] | HM.member fg colorz -> do
        let f = fgc fg
        pure $ niceTerm f s

      _  -> throwIO (BadFormException @c nil)

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

    entry $ bindMatch "str:stdin" $ \case
      [] -> liftIO getContents <&> mkStr @c

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:put" $ nil_ $ \case
      [LitStrVal s] -> liftIO $ TIO.putStr s
      _ -> throwIO (BadFormException @c nil)

    brief "reads file as a string" do
      entry $ bindMatch "str:file" $ \case
        [StringLike fn] -> liftIO (TIO.readFile fn) <&> mkStr

        _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "str:save" $ nil_ \case
      [StringLike fn, StringLike what] ->
        liftIO (writeFile fn what)

      _ -> throwIO (BadFormException @c nil)

    entry $ bindValue "space" $ mkStr " "

    let doParseTop w l s =
         parseTop s & either (const nil) (mkForm w . fmap ( l . fixContext) )

    let wrapWith e = \case
          List c es -> List c (e : es)
          other     -> other
    let lwrap = \case
          e@(SymbolVal x) -> wrapWith e
          _ -> id

    entry $ bindMatch "json" \case
      [ e ] -> pure $ mkStr $ LBS8.unpack $ Aeson.encode $ toJSON e
      x      -> pure $ mkStr $ LBS8.unpack $ Aeson.encode $ toJSON (mkList x)

    entry $ bindMatch "json:stdin" $ const do
      parseJson <$> liftIO (LBS.hGetContents stdin)

    entry $ bindMatch "json:file" $ \case
      [StringLike fn] -> do
        parseJson <$>  liftIO (LBS.readFile fn)

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "json:string" $ \case
      [TextLike s] -> do
        pure $ parseJson $ encodeUtf8 s & LBS.fromStrict

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "yaml:stdin" $ const do
      parseYaml <$> liftIO (LBS.hGetContents stdin)

    entry $ bindMatch "yaml:file" $ \case
      [StringLike fn] -> do
        parseYaml <$>  liftIO (LBS.readFile fn)

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "ini:stdin" $ const do
      parseIni <$> liftIO (LBS.hGetContents stdin)

    entry $ bindMatch "ini:file" $ \case
      [StringLike fn] -> do
        parseIni <$>  liftIO (LBS.readFile fn)

      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "top:stdin" $ const do
      liftIO TIO.getContents
          <&> either (const nil) (mkList . fmap fixContext) . parseTop

    entry $ bindMatch "top:file" $ \case
      [StringLike fn] -> do
        liftIO $ TIO.readFile fn
            <&> either (const nil) (mkList . fmap fixContext) . parseTop

      _ -> throwIO (BadFormException @c nil)

    brief "parses string as toplevel and produces a form"
     $ desc "parse:top:string SYMBOL STRING-LIKE"
     $ entry $ bindMatch "parse:top:string" $ \case

        [SymbolVal w, LitStrVal s] -> do
          pure $  doParseTop w id s

        [SymbolVal w, e@(SymbolVal r), LitStrVal s] -> do
          pure $  doParseTop w (lwrap e) s

        _ -> throwIO (BadFormException @c nil)

    brief "parses file as toplevel form and produces a form"
     $ desc "parse:top:file SYMBOL <FILENAME>"
     $ entry $ bindMatch "parse:top:file" $ \case

        [SymbolVal w, StringLike fn] -> do
          s <- liftIO $ TIO.readFile fn
          pure $  doParseTop w id s

        [SymbolVal w, e@(SymbolVal r), StringLike fn] -> do
          s <- liftIO $ TIO.readFile fn
          pure $  doParseTop w (lwrap e)  s

        _ -> throwIO (BadFormException @c nil)

    let atomFrom = \case
          [StringLike s] -> pure (mkSym s)
          [e]            -> pure (mkSym $ show $ pretty e)
          es             -> atomFrom [concatTerms hcat es]

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


    entry $ bindMatch "int" $ \case
      [ StringLike x ] -> pure $ maybe nil mkInt (readMay x)
      [ LitScientificVal v ] -> pure $ mkInt (round v)
      _ -> pure nil

    entry $ bindMatch "str" $ \case
      []  -> pure $ mkStr ""
      [x] -> pure $ mkStr (show $ pretty x)
      xs  -> pure $ mkStr $ mconcat [ show (pretty e) | e <- xs ]

    entry $ bindMatch "and" $ \case
      xs -> pure $ mkBool $ and [ not (isFalse x) | x <- xs ]

    entry $ bindMatch "or" $ \case
      xs -> pure $ mkBool $ or [ not (isFalse x) | x <- xs ]

    brief "compares two terms" $
      args [arg "term" "a", arg "term" "b"] $
      returns "boolean" "#t if terms are equal, otherwise #f" $
        entry $ bindMatch "eq?" $ \case
          [a, b] -> do
            pure $ if a == b then mkBool True else mkBool False
          _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "le?" $ \case
      [a, b] -> pure $ mkBool (compareSyn a b == LT)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "gt?" $ \case
      [a, b] -> pure $ mkBool (compareSyn a b == GT)
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "leq?" $ \case
      [a, b] -> pure $ mkBool (compareSyn a b /= GT)  -- LT или EQ
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "gte?" $ \case
      [a, b] -> pure $ mkBool (compareSyn a b /= LT)  -- GT или EQ
      _ -> throwIO (BadFormException @c nil)

    entry $ bindMatch "length" $ \case
      [ListVal es] -> pure $ mkInt (length es)
      [StringLike es] -> pure $ mkInt (length es)
      _ -> pure $ mkInt 0

    entry $ bindMatch "nil?" $ \case
      [ListVal []] -> pure $ mkBool True
      _ -> pure $ mkBool False

    entry $ bindMatch "not" $ \case
      [w] -> pure (mkBool (isTrue w))
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
           pure $  mkList [ mkList [mkSym @c a, mkStr b] | (a,b) <- s ]

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


    brief "decodes bytes as utf8 text"
     $ desc "bytes:decode <BYTES>"
     $ entry $ bindMatch "bytes:decode" $ \case
        [ OpaqueVal box ] -> do

          let lbs' = fromOpaque @LBS.ByteString box
                      <|>
                     (LBS.fromStrict <$> fromOpaque @BS.ByteString box)

          lbs <- maybe (throwIO (UnexpectedType "unknown / ByteString")) pure lbs'

          -- TODO: maybe-throw-on-invalid-encoding
          let txt = decodeUtf8With ignore (LBS.toStrict lbs)

          pure $ mkStr txt

        _ -> throwIO (BadFormException @c nil)


    brief "reads bytes from a file"
     $ desc "bytes:file FILE"
     $ entry $ bindMatch "bytes:file" $ \case
        [ StringLike fn ] -> do
          liftIO (LBS.readFile fn) >>= mkOpaque

        _ -> throwIO (BadFormException @c nil)

    brief "reads bytes from a STDIN"
     $ desc "bytes:stdin"
     $ entry $ bindMatch "bytes:stdin" $ \case
        [] -> do
          liftIO LBS.getContents >>= mkOpaque

        _ -> throwIO (BadFormException @c nil)

    brief "writes bytes to STDOUT"
     $ desc "bytes:put <BYTES>"
     $ entry $ bindMatch "bytes:put" $ nil_ $ \case
        [isOpaqueOf @LBS.ByteString -> Just s ] -> do
          liftIO $ LBS.putStr s

        [isOpaqueOf @ByteString -> Just s ] -> do
          liftIO $ BS.putStr s

        _ -> throwIO (BadFormException @c nil)

    brief "writes bytes to FILE"
     $ desc "bytes:write <FILE> <BYTES>"
     $ entry $ bindMatch "bytes:write" $ nil_ $ \case
        [StringLike fn, isOpaqueOf @LBS.ByteString -> Just s ] -> do
          liftIO $ LBS.writeFile fn s

        [StringLike fn, isOpaqueOf @ByteString -> Just s ] -> do
          liftIO $ BS.writeFile fn s

        _ -> throwIO (BadFormException @c nil)

    brief "calls external process"
      $ entry $ bindMatch "call:proc" \case
          [StringLike what] -> lift do
            callProc what mempty mempty <&> mkList @c . fmap (fixContext)

          (StringLike x:xs) -> lift do
            callProc x (fmap (show.pretty) xs) mempty <&> mkList @c . fmap (fixContext)

          _ -> throwIO (BadFormException @c nil)


    brief "calls external process"
      $ entry $ bindMatch "call:proc:raw" \case
          [StringLike what] -> lift do
            callProcRaw what mempty <&> mkStr @c

          (StringLike x:xs) -> lift do
            callProcRaw x (fmap (show.pretty) xs) <&> mkStr @c

          _ -> throwIO (BadFormException @c nil)


    brief "call external process as pipe"
      $ entry $ bindMatch "proc:pipe" \case

          [StringLike name, ListVal (StringLikeList params), TextLike input ] -> lift do
            mkStr @c <$> pipeProcText name params input

          _ -> throwIO (BadFormException @c nil)


    entry $ bindMatch "grep" \case
      [TextLike needle, what ] | matchOne needle what
        -> pure what

      [TextLike needle, e@(ListVal xs) ] | any (matchOne needle) xs ->
        pure $ mkList (filter (matchOne needle) xs)

      _ ->  pure nil

    entry $ bindMatch "pwd" $ const $ do
      pwd <&> mkSym @c

    entry $ bindMatch "cd" $ nil_ $ \case
      [ StringLike dir ] -> cd dir
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "mkdir" $ nil_ $ \case
      [ ListVal (StringLikeList p) ] -> do
        forM_ p mkdir

      (StringLikeList p) -> forM_ p mkdir

      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "rm" $ nil_ $ \case
      (StringLikeList p) -> forM_ p rm
      [ ListVal (StringLikeList p) ] -> forM_ p rm
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "mv" $ nil_ $ \case
      [ StringLike a, StringLike b ] -> mv a b
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "touch" $ nil_ $ \case
      [ StringLike p ] -> touch p
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "path:exists?" $ \case
      [ StringLike p ] -> lift do
        liftIO (Dir.doesPathExist p) <&> mkBool
      _ -> pure $ mkBool False

    entry $ bindMatch "path:dir?" $ \case
      [ StringLike p ] -> lift do
        liftIO (Dir.doesDirectoryExist p) <&> mkBool
      _ -> pure $ mkBool False

    entry $ bindMatch "path:file?" $ \case
      [ StringLike p ] -> lift do
        liftIO (Dir.doesFileExist p) <&> mkBool
      _ -> pure $ mkBool False

    entry $ bindMatch "path:ext" $ \case
      [ StringLike p ] -> pure $ mkSym (P.takeExtension p)
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "path:base" $ \case
      [ StringLike p ] -> pure $ mkSym (P.takeBaseName p)
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "path:dir" $ \case
      [ StringLike p ] -> pure $ mkSym (P.takeDirectory p)
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "path:expand" $ \case
      [ StringLike p ] -> lift do
        mkSym <$> canonicalizePath p
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "dir:list:files" $ \case
      [ StringLike p ] -> lift do
        dirFiles p <&> mkList . fmap mkSym
      _ -> throwIO $ BadFormException @c nil

    entry $ bindMatch "dir:list:all" $ \case
      [ StringLike p ] -> lift do
        what <- S.toList_ $ dirEntries p $ \e -> do
          let r = case e of
                   EntryFile   what -> mkList @c [mkSym what, mkSym "file" ]
                   EntryDir    what -> mkList @c [mkSym what, mkSym "dir"  ]
                   EntryOther  what -> mkList @c [mkSym what, mkSym "other" ]
          S.yield r
          pure True
        pure $ mkList what

      _ -> throwIO $ BadFormException @c nil


    entry $ bindMatch "strftime" $ \case
      [ StringLike fmt, LitIntVal t ] -> do
        let utcTime = posixSecondsToUTCTime (fromIntegral t)
            formattedTime = formatTime defaultTimeLocale fmt utcTime
        pure $ mkStr formattedTime

      _ -> pure $ mkStr  ""

    entry $ bindMatch "forked" $ \case
      [ e ] -> do
        env <- ask
        po <- pwd
        oe <- liftIO $ getEnvironment
        lift do
          flip runContT pure do

            a <- ContT $ withAsyncBound $ do
                    runEval env [e]

            r <- wait a

            cd po
            restoreEnvironment oe
            pure r

      _ -> throwIO $ BadFormException @c nil


    entry $ bindMatch "css" $ \case
        [ sel, ListVal kwa ] -> do

          let se = case sel of
                     ListVal es -> asSym $ concatTerms hcat $ List.intersperse (mkSym ",") es
                     TextLike s -> pretty $ mkSym @c s
                     other      -> pretty $ mkSym @c (show $ pretty other)

          let body = hsep
                       [ pretty k <> ":" <+> pretty v <> semi
                       | ListVal [TextLike k, v] <- kwa
                       ]

          let css = se <+> braces body

          pure $ mkStr (show css)

        _ -> pure nil

    entry $ bindMatch "html" $ \syn -> do

      let what = case syn of
            (TextLike tag : ListVal a : [ListVal content] )  -> Just (tag,a,content)
            (TextLike tag : ListVal a : content )  -> Just (tag,a,content)
            [TextLike tag] -> Just (tag,mempty,mempty)
            _ -> Nothing

      case what of

        Nothing -> pure nil

        Just (tag, a, content) -> do

          let attrs = [ Text.pack (show $ " " <> pretty k <> "=" <> dquotes (pretty (Html.text v)))
                      | ListVal [TextLike k, TextLike v] <- a
                      ] & mconcat

          let body = case concatTerms hsep (flattenList (mkList content)) of
                       TextLike s -> s
                       _          -> mempty

          let closing = if null content then mempty else angles ( "/" <> pretty tag )
          let wtf = angles (pretty tag <> pretty attrs) <> pretty body <> closing

          pure $ mkStr (show wtf)


parseJson :: forall c . IsContext c => LBS.ByteString -> Syntax c
parseJson input = case Aeson.decode @Value input of
    Just val -> mkSyntax @c val
    Nothing  -> nil

parseYaml :: forall c . IsContext c => LBS.ByteString -> Syntax c
parseYaml input =
  case Yaml.decodeEither' @Value (LBS.toStrict input) of
    Left _  -> nil @c
    Right val -> mkSyntax @c val

parseIni :: forall c . IsContext c => LBS.ByteString -> Syntax c
parseIni input =
  case Ini.parseIni (decodeUtf8With ignore $ LBS.toStrict  input) of
    Left _    -> nil
    Right ini -> mkSyntax @c (IniConfig ini)

matchOne :: IsContext c => Text -> Syntax c -> Bool
matchOne what = \case
  s@(TextLike x) | Text.isInfixOf what x -> True
  e@(ListVal xs) -> or [ Text.isInfixOf what s | TextLike s <- xs ]
  _ -> False

flattenList :: IsContext c => Syntax c -> [Syntax c]
flattenList (ListVal xs) = concatMap flattenList xs
flattenList x = [x]

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM p (x:xs) = do
  (yes, no) <- partitionM p xs
  b <- p x
  pure $ if b then (x:yes, no) else (yes, x:no)

groupByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM _ [] = pure []
groupByM eq (x:xs) = do
  (same, rest) <- partitionM (eq x) xs
  groups <- groupByM eq rest
  pure ((x:same) : groups)

toOrdering :: Bool -> Ordering
toOrdering True  = LT
toOrdering False = GT

sortByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
sortByM cmp xs = do
  let indexed = zip xs [0..]

  keyVals <- mapM (\(a, i) -> do
                      k <- mapM (\b -> cmp a b) xs
                      pure (sum (map fromEnum k), i, a))
                  indexed

  let sorted = List.sortOn (\(key, idx, _) -> (key, idx)) keyVals

  pure $ map (\(_, _, val) -> val) sorted

compareSyn :: Syntax c -> Syntax c -> Ordering
compareSyn (LitIntVal a) (LitIntVal b) = compare a b
compareSyn (LitScientificVal a) (LitScientificVal b) = compare a b
compareSyn (LitIntVal a) (LitScientificVal b) = compare (fromIntegral a) b
compareSyn (LitScientificVal a) (LitIntVal b) = compare a (fromIntegral b)
compareSyn (TextLike a) (TextLike b) = compare a b
compareSyn (ListVal a) (ListVal b) = compareLists a b
compareSyn _ _ = error "type check error"

compareLists :: [Syntax c] -> [Syntax c] -> Ordering
compareLists [] [] = EQ
compareLists [] _  = LT
compareLists _  [] = GT
compareLists (x:xs) (y:ys) =
  case compareSyn x y of
    EQ -> compareLists xs ys
    ord -> ord

concatTerms :: forall ann c . IsContext c => ( [Doc ann] -> Doc ann) ->  [Syntax c] -> Syntax c
concatTerms s =  \case
  [ListVal xs] -> do
     mkStr @c  ( show $ pretty $ concatTerms s xs )

  xs -> mkStr ( show $ s (fmap fmt xs) )

asSym :: forall ann c . IsContext c => Syntax c -> Doc ann
asSym = \case
  TextLike s -> pretty (mkSym @c s)
  other      -> pretty other

restoreEnvironment :: MonadIO  m => [(String, String)] -> m ()
restoreEnvironment newEnv = liftIO do
    currentEnv <- getEnvironment
    let toRemove = map fst currentEnv \\ map fst newEnv
    mapM_ unsetEnv toRemove
    mapM_ (uncurry setEnv) newEnv


