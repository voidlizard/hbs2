module Fixme.Types where

import Fixme.Prelude

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Word (Word64)
import Data.Maybe
import Data.Coerce
import System.FilePath


data GitLocation =
  GitLocation
  { gitLocationHash   :: GitHash
  , gitLocationLine   :: Integer
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

data FixmeSource =
  FixmeSourceGit GitLocation
  deriving stock (Show,Data,Generic)

newtype FixmeTag = FixmeTag { fromFixmeTag :: Text }
                   deriving newtype (Eq,Ord,Show,IsString,Hashable,Semigroup,Monoid)
                   deriving stock (Data,Generic)

newtype FixmeTitle = FixmeTitle { fromFixmeTitle :: Text }
                     deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid)
                     deriving stock (Data,Generic)

newtype FixmePlainLine = FixmePlainLine { fromFixmeText :: Text }
                         deriving newtype (Eq,Ord,Show,IsString,Semigroup,Monoid)
                         deriving stock (Data,Generic)


newtype FixmeAttrName = FixmeAttrName { fromFixmeAttrName :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable)
                        deriving stock (Data,Generic)


newtype FixmeAttrVal = FixmeAttrVal { fromFixmeAttrVal :: Text }
                        deriving newtype (Eq,Ord,Show,IsString)
                        deriving stock (Data,Generic)


newtype FixmeTimestamp = FixmeTimestamp Word64
                        deriving newtype (Eq,Ord,Show,Num)
                        deriving stock (Data,Generic)

data Fixme =
  Fixme
  { fixmeTag    :: FixmeTag
  , fixmeTitle  :: FixmeTitle
  , fixmeTs     :: Maybe FixmeTimestamp
  , fixmePlain  :: [FixmePlainLine]
  , fixmeAttr   :: HashMap FixmeAttrName FixmeAttrVal
  , fixmeSource :: Maybe FixmeSource
  }
  deriving stock (Show,Data,Generic)


type FixmePerks m = ( MonadUnliftIO m
                    , MonadIO m
                    )


data FixmeEnv =
  FixmeEnv
  { fixmeEnvGitDir       :: Maybe FilePath
  , fixmeEnvFileMask     :: TVar [FilePattern]
  , fixmeEnvTags         :: TVar (HashSet FixmeTag)
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

newtype FixmeM m a = FixmeM { fromFixmeM :: ReaderT FixmeEnv m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadReader FixmeEnv
                                      )

runFixmeCLI :: FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  env <- FixmeEnv Nothing
            <$>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defCommentMap
            <*>  newTVarIO Nothing

  runReaderT ( setupLogger >> fromFixmeM m ) env
                 `finally` flushLoggers
  where
    setupLogger = do
      setLogging @ERROR  $ toStderr . logPrefix "[error] "
      setLogging @WARN   $ toStderr . logPrefix "[warn] "
      setLogging @NOTICE $ toStdout . logPrefix ""
      pure ()

    flushLoggers = do
      setLoggingOff @DEBUG
      setLoggingOff @ERROR
      setLoggingOff @WARN
      setLoggingOff @NOTICE

instance Serialise GitLocation
instance Serialise FixmeSource
instance Serialise FixmeTag
instance Serialise FixmeTitle
instance Serialise FixmePlainLine
instance Serialise FixmeAttrName
instance Serialise FixmeAttrVal
instance Serialise FixmeTimestamp
instance Serialise Fixme


instance Pretty FixmeTitle where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmeTag where
  pretty = pretty . coerce @_ @Text

instance Pretty FixmePlainLine where
  pretty = pretty . coerce @_ @Text

instance Pretty Fixme where
  pretty Fixme{..} =
    pretty fixmeTag <+> pretty fixmeTitle
    <> lls
    where
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


