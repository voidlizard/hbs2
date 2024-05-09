module Fixme.Types where

import Fixme.Prelude

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Word (Word64)
import Data.Coerce

data GitLocation =
  GitLocation
  { gitLocationHash   :: GitHash
  , gitLocationOffset :: Integer
  , gitLocationLength :: Integer
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

data FixmeSource =
  FixmeSourceGit GitLocation
  deriving stock (Show,Data,Generic)

newtype FixmeTag = FixmeTag { fromFixmeTag :: Text }
                   deriving newtype (Eq,Ord,Show,IsString,Hashable)
                   deriving stock (Data,Generic)

newtype FixmeTitle = FixmeTitle { fromFixmeTitle :: Text }
                     deriving newtype (Eq,Ord,Show,IsString)
                     deriving stock (Data,Generic)

newtype FixmePlainLine = FixmePlainLine { fromFixmeText :: Text }
                         deriving newtype (Eq,Ord,Show,IsString)
                         deriving stock (Data,Generic)


newtype FixmeAttrName = FixmeAttrName { fromFixmeAttrName :: Text }
                        deriving newtype (Eq,Ord,Show,IsString,Hashable)
                        deriving stock (Data,Generic)


newtype FixmeAttrVal = FixmeAttrVal { fromFixmeAttrVal :: Text }
                        deriving newtype (Eq,Ord,Show,IsString)
                        deriving stock (Data,Generic)


newtype FixmeTimestamp = FixmeTimestamp Word64
                        deriving newtype (Eq,Ord,Show)
                        deriving stock (Data,Generic)

data Fixme =
  Fixme
  { fixmeTag    :: FixmeTag
  , fixmeTitle  :: FixmeTitle
  , fixmeTs     :: FixmeTimestamp
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
  { fixmeEnvGitDir      :: Maybe FilePath
  , fixmeEnvFileMask    :: TVar [FilePattern]
  , fixmeEnvTags        :: TVar (HashSet FixmeTag)
  , fixmeEnvGitScanDays :: TVar (Maybe Integer)
  }

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

