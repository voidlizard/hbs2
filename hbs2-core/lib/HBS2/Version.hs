{-# LANGUAGE StrictData #-}

module HBS2.Version (inlineBuildVersion, BuildVersion(..)) where

import Data.Aeson
import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version, showVersion)
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..), dataToExpQ)
import System.Environment (getEnvironment)

data BuildVersion = BuildVersion
    { buildVersion_git :: Maybe Text
    , buildVersion_pkg :: Text
    }
    deriving (Generic, Eq, Show, Data)

instance ToJSON BuildVersion

-- | Lift text as expression in TH.
-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: (Data a) => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)

lookupEnv :: String -> Q (Maybe Text)
lookupEnv key = fmap (T.strip . T.pack) . lookup key <$> runIO getEnvironment

inlineBuildVersion :: Version -> Q Exp
inlineBuildVersion version =
    flip sigE [t|BuildVersion|] . liftDataWithText =<< do
        buildVersion_git <- lookupEnv "GIT_HASH"
        let buildVersion_pkg = (T.pack . showVersion) version
        pure BuildVersion {..}
