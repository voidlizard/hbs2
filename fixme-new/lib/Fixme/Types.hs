{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.Types where

import Fixme.Prelude

import DBPipe.SQLite
import HBS2.Git.Local

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Word (Word64,Word32)
import Data.Maybe
import Data.Coerce
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)


newtype FixmeTag = FixmeTag { fromFixmeTag :: Text }
                   deriving newtype (Eq,Ord,Show,IsString,Hashable,Semigroup,Monoid,ToField,FromField)
                   deriving stock (Data,Generic)

newtype FixmeTitle = FixmeTitle { fromFixmeTitle :: Text }
                     deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid,ToField,FromField)
                     deriving stock (Data,Generic)

newtype FixmePlainLine = FixmePlainLine { fromFixmeText :: Text }
                         deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid,ToField,FromField)
                         deriving stock (Data,Generic)


newtype FixmeAttrName = FixmeAttrName { fromFixmeAttrName :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable,ToField,FromField)
                        deriving stock (Data,Generic)


newtype FixmeAttrVal = FixmeAttrVal { fromFixmeAttrVal :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable,ToField,FromField)
                        deriving stock (Data,Generic)

newtype FixmeTimestamp = FixmeTimestamp Word64
                        deriving newtype (Eq,Ord,Show,Num,ToField,FromField)
                        deriving stock (Data,Generic)


newtype FixmeOffset = FixmeOffset Word32
                     deriving newtype (Eq,Ord,Show,Num,ToField,FromField)
                     deriving stock (Data,Generic)


data Fixme =
  Fixme
  { fixmeTag       :: FixmeTag
  , fixmeTitle     :: FixmeTitle
  , fixmeTs        :: Maybe FixmeTimestamp
  , fixmeStart     :: Maybe FixmeOffset
  , fixmeEnd       :: Maybe FixmeOffset
  , fixmePlain     :: [FixmePlainLine]
  , fixmeAttr      :: HashMap FixmeAttrName FixmeAttrVal
  }
  deriving stock (Show,Data,Generic)


type FixmePerks m = ( MonadUnliftIO m
                    , MonadIO m
                    )


data FixmeEnv =
  FixmeEnv
  { fixmeEnvGitDir       :: Maybe FilePath
  , fixmeEnvDb           :: DBPipeEnv
  , fixmeEnvFileMask     :: TVar [FilePattern]
  , fixmeEnvTags         :: TVar (HashSet FixmeTag)
  , fixmeEnvAttribs      :: TVar (HashSet FixmeAttrName)
  , fixmeEnvAttribValues :: TVar (HashMap FixmeAttrName (HashSet FixmeAttrVal))
  , fixmeEnvDefComments  :: TVar (HashSet Text)
  , fixmeEnvFileComments :: TVar (HashMap FilePath (HashSet Text))
  , fixmeEnvGitScanDays  :: TVar (Maybe Integer)
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

fixmeGetGitDirCLIOpt :: MonadReader FixmeEnv m => m String
fixmeGetGitDirCLIOpt = do
  asks fixmeEnvGitDir
      <&> fmap (\d -> [qc|--dir-dir {d}|])
      <&> fromMaybe ""

newtype FixmeM m a = FixmeM { fromFixmeM :: ReaderT FixmeEnv m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadReader FixmeEnv
                                      )

withFixmeEnv :: FixmePerks m => FixmeEnv -> FixmeM m a -> m a
withFixmeEnv env what = runReaderT ( fromFixmeM what) env

instance Serialise FixmeTag
instance Serialise FixmeTitle
instance Serialise FixmePlainLine
instance Serialise FixmeAttrName
instance Serialise FixmeAttrVal
instance Serialise FixmeTimestamp
instance Serialise FixmeOffset
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


