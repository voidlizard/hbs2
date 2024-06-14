{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.Types
  ( module Fixme.Types
  ) where

import Fixme.Prelude hiding (align)
import HBS2.Base58

import DBPipe.SQLite hiding (field)
import HBS2.Git.Local

import Data.Config.Suckless

import Prettyprinter.Render.Terminal
import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Word (Word64,Word32)
import Data.Maybe
import Data.Coerce
import Data.Text qualified as Text
import Data.List qualified as List
import Data.Map qualified as Map
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)
import Data.Generics.Product.Fields (field)
import Lens.Micro.Platform

-- FIXME: move-to-suckless-conf
deriving stock instance Ord (Syntax C)

pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)

pattern FixmeHashLike :: forall {c} . Text -> Syntax c
pattern FixmeHashLike  e <- (fixmeHashFromSyn -> Just e)


pattern TimeStampLike :: forall {c} . FixmeTimestamp -> Syntax c
pattern TimeStampLike  e <- (tsFromFromSyn -> Just e)

fixContext :: IsContext c => Syntax c -> Syntax C
fixContext = go
  where
    go = \case
      List    _ xs -> List noContext (fmap go xs)
      Symbol  _ w  -> Symbol noContext w
      Literal _ l  -> Literal noContext l

mklist :: IsContext c => [Syntax c] -> Syntax c
mklist = List noContext

mkint :: (IsContext c, Integral a) => a -> Syntax c
mkint  = Literal noContext . LitInt . fromIntegral

mksym :: IsContext c => Id -> Syntax c
mksym  = Symbol noContext

class MkId a where
  mkId :: a -> Id

instance MkId FixmeAttrName where
  mkId (k :: FixmeAttrName) = Id ("$" <> coerce k)

instance MkId (Text,Int) where
  mkId (p, i) = Id (p <> fromString (show i))

instance MkId (String,Integer) where
  mkId (p, i) = Id (fromString p <> fromString (show i))

class IsContext c => MkStr c a where
  mkstr :: a -> Syntax c


instance IsContext c => MkStr c String where
  mkstr s = Literal (noContext @c) (LitStr $ Text.pack s)

instance IsContext c => MkStr c ByteString where
  mkstr s = Literal (noContext @c) (LitStr $ Text.pack $ BS8.unpack s)

instance IsContext c => MkStr c (Maybe FixmeKey) where
  mkstr Nothing  = Literal (noContext @c) (LitStr "")
  mkstr (Just k) = Literal (noContext @c) (LitStr (coerce k))

instance IsContext c => MkStr c FixmeAttrVal where
  mkstr (s :: FixmeAttrVal)  = Literal (noContext @c) (LitStr (coerce s))


instance IsContext c => MkStr c (Maybe FixmeAttrVal) where
  mkstr (Just v) = mkstr v
  mkstr Nothing  = mkstr ( "" :: Text )

instance IsContext c => MkStr c FixmeAttrName where
  mkstr (s :: FixmeAttrName)  = Literal (noContext @c) (LitStr (coerce s))

instance IsContext c => MkStr c HashRef where
  mkstr s  = Literal (noContext @c) (LitStr (fromString $ show $ pretty s))

instance IsContext c => MkStr c Text where
  mkstr = Literal noContext . LitStr

stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes

fixmeHashFromSyn :: Syntax c -> Maybe Text
fixmeHashFromSyn = \case
  StringLike s -> do
    let (_,value) = span (`elem` "#%~:") s
    Just $ Text.pack value

  _            -> Nothing

tsFromFromSyn :: Syntax c -> Maybe FixmeTimestamp
tsFromFromSyn = \case
  LitIntVal n -> Just (fromIntegral n)
  _ -> Nothing

newtype FixmeTag = FixmeTag { fromFixmeTag :: Text }
                   deriving newtype (Eq,Ord,Show,IsString,Hashable,Semigroup,Monoid,ToField,FromField)
                   deriving stock (Data,Generic)

newtype FixmeTitle = FixmeTitle { fromFixmeTitle :: Text }
                     deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid,ToField,FromField,Hashable)
                     deriving stock (Data,Generic)

newtype FixmePlainLine = FixmePlainLine { fromFixmeText :: Text }
                         deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid,ToField,FromField)
                         deriving stock (Data,Generic)


newtype FixmeAttrName = FixmeAttrName { fromFixmeAttrName :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable)
                        deriving newtype (ToField,FromField)
                        deriving newtype (ToJSON,FromJSON,ToJSONKey,FromJSONKey)
                        deriving stock (Data,Generic)


newtype FixmeAttrVal = FixmeAttrVal { fromFixmeAttrVal :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable,ToField,FromField,ToJSON,FromJSON,Semigroup,Monoid)
                        deriving stock (Data,Generic)

newtype FixmeTimestamp = FixmeTimestamp Word64
                        deriving newtype (Eq,Ord,Show,Num,ToField,FromField)
                        deriving stock (Data,Generic)


newtype FixmeKey  = FixmeKey Text
                    deriving newtype (Eq,Ord,Show,ToField,FromField)
                    deriving stock (Data,Generic)

newtype FixmeOffset = FixmeOffset Word32
                     deriving newtype (Eq,Ord,Show,Num,ToField,FromField)
                     deriving newtype (Integral,Real,Enum)
                     deriving stock (Data,Generic)


data Fixme =
  Fixme
  { fixmeTag       :: FixmeTag
  , fixmeTitle     :: FixmeTitle
  , fixmeKey       :: Maybe FixmeKey
  , fixmeTs        :: Maybe FixmeTimestamp
  , fixmeStart     :: Maybe FixmeOffset
  , fixmeEnd       :: Maybe FixmeOffset
  , fixmePlain     :: [FixmePlainLine]
  , fixmeAttr      :: HashMap FixmeAttrName FixmeAttrVal
  }
  deriving stock (Ord,Eq,Show,Data,Generic)

instance Monoid Fixme where
  mempty = Fixme mempty mempty Nothing Nothing Nothing Nothing mempty mempty

instance Semigroup Fixme where
  (<>) a b = b { fixmeTs  = fixmeTs b <|> fixmeTs a
               , fixmeTitle = fixmeAttrNonEmpty (fixmeTitle a) (fixmeTitle b)
               , fixmeTag   = fixmeAttrNonEmpty (fixmeTag a) (fixmeTag b)
               , fixmeStart = fixmeStart b <|> fixmeStart a
               , fixmeEnd = fixmeEnd b <|> fixmeEnd a
               , fixmePlain = fixmePlain b
               , fixmeAttr = fixmeAttr a <> fixmeAttr b
               }

newtype FixmeThin = FixmeThin (HashMap FixmeAttrName FixmeAttrVal)
                    deriving newtype (Semigroup,Monoid,Eq,Ord,Show,ToJSON,FromJSON)
                    deriving stock (Data,Generic)




type FixmePerks m = ( MonadUnliftIO m
                    , MonadIO m
                    )


data UpdateAction = forall c . IsContext c => UpdateAction { runUpdateAction :: Syntax c -> IO () }

data ReadLogAction = forall c . IsContext c => ReadLogAction { runReadLog :: Syntax c -> IO () }

-- FIXME: fucking-context-hardcode-wtf-1
data CatAction = CatAction { catAction :: [(Id, Syntax C)] -> LBS.ByteString -> IO () }

data SimpleTemplate = forall c . (IsContext c, Data (Context c), Data c) => SimpleTemplate [Syntax c]

class HasSequence w where
  getSequence :: w -> Word64

data CompactAction =
    Deleted  Word64 HashRef
  | Modified Word64 HashRef FixmeAttrName FixmeAttrVal
  | Added    Word64 Fixme
  deriving stock (Eq,Ord,Show,Generic)

class MkKey a where
  mkKey :: a -> ByteString

instance MkKey CompactAction where
  mkKey (Deleted _ h) = "D" <> LBS.toStrict (serialise h)
  mkKey (Modified _ h _ _) = "M" <> LBS.toStrict (serialise h)
  mkKey (Added _ fixme) = "A" <> coerce (hashObject @HbSync $ serialise fixme)

instance Pretty CompactAction where
  pretty = \case
    Deleted s r      -> pretty $ mklist @C [ mksym "deleted", mkint s, mkstr r  ]
    Modified s r k v -> pretty $ mklist @C [ mksym "modified", mkint s, mkstr r, mkstr k, mkstr v ]
    -- FIXME: normal-pretty-instance
    e@(Added  w fx) -> do
      pretty $ mklist @C [ mksym "added", mkstr (toBase58 $ mkKey e) ]

instance Serialise CompactAction

pattern CompactActionSeq :: Word64 -> CompactAction
pattern CompactActionSeq s <- (seqOf -> Just s)

{-# COMPLETE CompactActionSeq #-}

seqOf :: CompactAction -> Maybe Word64
seqOf = \case
  Deleted   w  _ -> Just w
  Modified  w _ _ _ -> Just w
  Added w _ -> Just w

instance HasSequence CompactAction where
  getSequence x = fromMaybe 0 (seqOf x)

data FixmeTemplate =
  Simple SimpleTemplate

data RenderError = RenderError String
                   deriving stock (Eq,Show,Typeable)

class FixmeRenderTemplate a b where
  render :: a -> Either RenderError b

data FixmeOpts =
  FixmeOpts
  { fixmeOptNoEvolve :: Bool
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

instance Monoid FixmeOpts where
  mempty = FixmeOpts False

instance Semigroup FixmeOpts where
  (<>) _ b = FixmeOpts (fixmeOptNoEvolve b)

data FixmeEnv =
  FixmeEnv
  { fixmeLock              :: MVar ()
  , fixmeEnvOpts           :: TVar FixmeOpts
  , fixmeEnvDbPath         :: TVar FilePath
  , fixmeEnvDb             :: TVar (Maybe DBPipeEnv)
  , fixmeEnvGitDir         :: TVar (Maybe FilePath)
  , fixmeEnvFileMask       :: TVar [FilePattern]
  , fixmeEnvTags           :: TVar (HashSet FixmeTag)
  , fixmeEnvAttribs        :: TVar (HashSet FixmeAttrName)
  , fixmeEnvAttribValues   :: TVar (HashMap FixmeAttrName (HashSet FixmeAttrVal))
  , fixmeEnvDefComments    :: TVar (HashSet Text)
  , fixmeEnvFileComments   :: TVar (HashMap FilePath (HashSet Text))
  , fixmeEnvGitScanDays    :: TVar (Maybe Integer)
  , fixmeEnvUpdateActions  :: TVar [UpdateAction]
  , fixmeEnvReadLogActions :: TVar [ReadLogAction]
  , fixmeEnvCatAction      :: TVar CatAction
  , fixmeEnvTemplates      :: TVar (HashMap Id FixmeTemplate)
  , fixmeEnvMacro          :: TVar (HashMap Id (Syntax C))
  , fixmeEnvCatContext     :: TVar (Int,Int)
  }


fixmeGetCommentsFor :: FixmePerks m => Maybe FilePath -> FixmeM m [Text]

fixmeGetCommentsFor Nothing = do
  asks fixmeEnvDefComments >>= readTVarIO
    <&> HS.toList

fixmeGetCommentsFor (Just fp) = do
  cof <- asks fixmeEnvFileComments >>= readTVarIO
  def <- asks fixmeEnvDefComments >>= readTVarIO

  let r = maybe mempty HS.toList (HM.lookup (commentKey fp) cof)
           <> HS.toList def

  pure r

{- HLINT ignore "Functor law" -}

fixmeGetGitDirCLIOpt :: (FixmePerks m, MonadReader FixmeEnv m) => m String
fixmeGetGitDirCLIOpt = do
  asks fixmeEnvGitDir
          >>= readTVarIO
          <&> fmap (\d -> [qc|--git-dir {d}|])
          <&> fromMaybe ""

newtype FixmeM m a = FixmeM { fromFixmeM :: ReaderT FixmeEnv m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadReader FixmeEnv
                                      )


fixmeEnvBare :: FixmePerks m => m FixmeEnv
fixmeEnvBare =
  FixmeEnv
    <$>  newMVar ()
    <*>  newTVarIO mempty
    <*>  newTVarIO ":memory:"
    <*>  newTVarIO Nothing
    <*>  newTVarIO Nothing
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO defCommentMap
    <*>  newTVarIO Nothing
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO (CatAction $ \_ _ -> pure ())
    <*>  newTVarIO mempty
    <*>  newTVarIO mempty
    <*>  newTVarIO (1,3)

withFixmeEnv :: FixmePerks m => FixmeEnv -> FixmeM m a -> m a
withFixmeEnv env what = runReaderT ( fromFixmeM what) env

-- FIXME: move-to-suckless-conf-library
deriving newtype instance Hashable Id

instance Serialise FixmeTag
instance Serialise FixmeTitle
instance Serialise FixmePlainLine
instance Serialise FixmeAttrName
instance Serialise FixmeAttrVal
instance Serialise FixmeTimestamp
instance Serialise FixmeOffset
instance Serialise FixmeKey
instance Serialise Fixme


instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField GitRef where
  toField h = toField (show $ pretty h)

instance FromField GitRef where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String

instance Pretty FixmeTimestamp where
  pretty = pretty . coerce @_ @Word64

instance Pretty FixmeOffset where
  pretty = pretty . coerce @_ @Word32

instance Pretty FixmeAttrName where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmeAttrVal where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmeTitle where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmeTag where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmePlainLine where
  pretty = pretty . coerce @_ @Text

instance Pretty Fixme where
  pretty Fixme{..} =
    pretty fixmeTag <+> pretty fixmeTitle
    <> fstart
    <> fend
    <> la
    <> lls
    <> line
    where

      fstart = case fixmeStart of
        Just s  -> line <> pretty ([qc| $fixme-start: {show $ pretty s}|] :: String)
        Nothing -> mempty

      fend = case fixmeEnd of
        Just s  -> line <> pretty ([qc| $fixme-end: {show $ pretty s}|] :: String)
        Nothing -> mempty

      la | not (HM.null fixmeAttr) = do
            let a = HM.toList fixmeAttr
            let ss = [ [qc| ${show $ pretty n}: {show $ pretty v}|]  | (n,v) <- a ]  :: [String]
            line <> vcat ( fmap pretty ss ) <> line

         | otherwise = mempty

      lls | not (null fixmePlain) =  line <> vcat (fmap pretty fixmePlain)
          | otherwise = mempty


defCommentMap :: HashMap FilePath (HashSet Text)
defCommentMap = HM.fromList
  [ comment ".cabal"   ["--"]
  , comment ".hs"      ["--"]
  , comment ".c"       ["//"]
  , comment ".h"       ["//"]
  , comment ".cc"      ["//"]
  , comment ".cpp"     ["//"]
  , comment ".cxx"     ["//"]
  , comment "Makefile" ["#"]
  ]
  where
    comment a b = (a, HS.fromList b)

commentKey :: FilePath -> FilePath
commentKey fp =
  case takeExtension fp of
    "" -> takeFileName fp
    xs -> xs

type ContextShit c = (Data c, Data (Context c), IsContext c, Data (Syntax c))


cc0 :: forall c . ContextShit c => Context c
cc0 = noContext :: Context c

inject :: forall c  a . (ContextShit c, Data a) => [(Id,Syntax c)] -> a -> a
inject repl target =
  flip transformBi target $ \case
   (SymbolVal x) | issubst x -> fromMaybe mt (Map.lookup x rmap)
   other -> other
   where
    mt = Literal (noContext @c) (LitStr "")
    rmap = Map.fromList repl
    issubst (Id x) = Text.isPrefixOf "$" x

pattern NL :: forall {c}. Syntax c
pattern NL <- ListVal [SymbolVal "nl"]


instance FixmeRenderTemplate SimpleTemplate (Doc AnsiStyle) where

  render (SimpleTemplate syn) = Right $ mconcat $
      flip fix (mempty,syn) $ \next -> \case
        (acc, NL : rest)    -> next (acc <> nl, rest)
        (acc, ListVal [StringLike w] : rest) -> next (acc <> txt w, rest)
        (acc, StringLike w : rest) -> next (acc <> txt w, rest)
        (acc, ListVal [SymbolVal "trim", LitIntVal n, e] : rest) -> next (acc <> trim n (deep' [e]), rest)
        (acc, ListVal [SymbolVal "align", LitIntVal n, e] : rest) -> next (acc <> align n (deep' [e]), rest)
        (acc, ListVal [SymbolVal "fg", SymbolVal co, e] : rest) -> next (acc <> fmap (fg_ (color_ co)) (deep [e]), rest)
        (acc, ListVal [SymbolVal "bg", SymbolVal co, e] : rest) -> next (acc <> fmap (bg_ (color_ co)) (deep [e]), rest)
        (acc, ListVal [SymbolVal "fgd", SymbolVal co, e] : rest) -> next (acc <> fmap (fgd_ (color_ co)) (deep [e]), rest)
        (acc, ListVal [SymbolVal "bgd", SymbolVal co, e] : rest) -> next (acc <> fmap (bgd_ (color_ co)) (deep [e]), rest)

        (acc, ListVal [ SymbolVal "if", cond
                      , ListVal (SymbolVal "then" : then_)
                      , ListVal (SymbolVal "else" : else_)
                      ] : rest) -> do

          let r = case cond of
                    ListVal [SymbolVal "~", StringLike p, evaluated -> Just x] ->
                      Text.isPrefixOf (Text.pack p) x
                    _ -> False

          next (acc <> if r then deep then_ else deep else_, rest)


        (acc, ListVal es : rest) ->  next (acc <> deep es, rest)
        (acc, e : rest) -> next (acc <> p e, rest)
        (acc, []) -> acc

    where

      evaluated :: (ContextShit c) => Syntax c -> Maybe  Text
      evaluated what = Just (deep' [what] & Text.concat)

      color_ = \case
        "black"   -> Just Black
        "red"     -> Just Red
        "green"   -> Just Green
        "yellow"  -> Just Yellow
        "blue"    -> Just Blue
        "magenta" -> Just Magenta
        "cyan"    -> Just Cyan
        "white"   -> Just White
        _         -> Nothing


      fg_ = maybe id (annotate . color)
      bg_ = maybe id (annotate . bgColor)

      fgd_ = maybe id (annotate . colorDull)
      bgd_ = maybe id (annotate . bgColorDull)

      untxt = fmap pretty

      align n0 s0 | n > 0     = untxt [Text.justifyLeft n ' ' s]
                  | otherwise = untxt [Text.justifyRight (abs n) ' ' s]

        where
          n = fromIntegral n0
          s = mconcat s0

      trim n0 s0 | n >= 0     = untxt [ Text.take n s ]
                 | otherwise  = untxt [ Text.takeEnd (abs n) s ]
        where
          n = fromIntegral n0
          s = mconcat s0

      deep :: forall c . (ContextShit c) => [Syntax c] -> [Doc AnsiStyle]
      deep sy = either mempty List.singleton (render (SimpleTemplate sy))

      deep' :: forall c . (ContextShit c) => [Syntax c] -> [Text]
      deep' sy = do
        let what = deep sy
        [ Text.pack (show x) | x <- what]

      nl = [ line ]
      txt s = [fromString s]
      p e = untxt [Text.pack (show $ pretty e)]


instance FixmeRenderTemplate SimpleTemplate Text where
  render (SimpleTemplate syn) = Right $ Text.concat $
      flip fix (mempty,syn) $ \next -> \case
        (acc, NL : rest)    -> next (acc <> nl, rest)
        (acc, ListVal [StringLike w] : rest) -> next (acc <> txt w, rest)
        (acc, StringLike w : rest) -> next (acc <> txt w, rest)
        (acc, ListVal [SymbolVal "trim", LitIntVal n, e] : rest) -> next (acc <> trim n (deep [e]), rest)
        (acc, ListVal [SymbolVal "align", LitIntVal n, e] : rest) -> next (acc <> align n (deep [e]), rest)
        (acc, ListVal es : rest) ->  next (acc <> deep es, rest)
        (acc, e : rest) -> next (acc <> p e, rest)
        (acc, []) -> acc

    where

      align n0 s0 | n > 0     = [Text.justifyLeft n ' ' s]
                  | otherwise = [Text.justifyRight (abs n) ' ' s]

        where
          n = fromIntegral n0
          s = mconcat s0

      trim n0 s0 | n >= 0     = [ Text.take n s ]
                 | otherwise  = [ Text.takeEnd (abs n) s ]
        where
          n = fromIntegral n0
          s = mconcat s0

      deep :: forall c . (ContextShit c) => [Syntax c] -> [Text]
      deep sy = either mempty List.singleton (render (SimpleTemplate sy))

      nl = [ "\n" ]
      txt s = [fromString s]
      p e = [Text.pack (show $ pretty e)]


newtype ViaSerialise a = ViaSerialise a

instance Serialise a => Hashed HbSync (ViaSerialise a) where
  hashObject (ViaSerialise x) = hashObject (serialise x)


fixmeTitleNonEmpty :: FixmeTitle -> FixmeTitle -> FixmeTitle
fixmeTitleNonEmpty a b = case (coerce a :: Text, coerce b :: Text) of
  (x,y) | Text.null x && not (Text.null y) -> FixmeTitle y
  (x,y) | not (Text.null x) &&  Text.null y -> FixmeTitle x
  (_,y) -> FixmeTitle y

fixmeAttrNonEmpty :: Coercible a Text => a -> a -> a
fixmeAttrNonEmpty a b = case (coerce a :: Text, coerce b :: Text) of
  (x,y) | Text.null x && not (Text.null y) -> b
  (x,y) | not (Text.null x) &&  Text.null y -> a
  (_,_) -> b

fixmeDerivedFields :: Fixme -> Fixme
fixmeDerivedFields fx = fx <> fxCo <> tag <> fxLno <> fxMisc
  where
    email = HM.lookup "commiter-email" (fixmeAttr fx)
              & maybe mempty (\x -> " <" <> x <> ">")

    comitter = HM.lookup "commiter-name" (fixmeAttr fx)
                 <&> (<> email)

    tag = mempty { fixmeAttr = HM.singleton "fixme-tag" (FixmeAttrVal (coerce $ fixmeTag fx))  }

    lno = succ <$> fixmeStart fx <&> FixmeAttrVal . fromString . show

    fxLno = mempty { fixmeAttr = maybe mempty (HM.singleton "line") lno }

    fxCo =
      maybe mempty (\c -> mempty { fixmeAttr = HM.singleton "committer" c }) comitter

    fxMisc =
      fx & over (field @"fixmeAttr")
                (HM.insert "fixme-title" (FixmeAttrVal (coerce (fixmeTitle fx))))

mkFixmeFileName :: FilePath -> Fixme
mkFixmeFileName fp =
  mempty { fixmeAttr = HM.singleton "file" (FixmeAttrVal (fromString fp)) }


