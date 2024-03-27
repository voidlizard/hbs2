module HBS2.Git.Oracle.Facts where

import HBS2.Git.Oracle.Prelude
import Data.Word
import Codec.Serialise

type PKS = PubKey 'Sign HBS2Basic

deriving instance Data (RefLogKey HBS2Basic)
deriving instance Data (LWWRefKey HBS2Basic)

data GitRepoRefFact =
  GitRepoFact1
  { gitLwwRef :: LWWRefKey HBS2Basic
  , gitLwwSeq :: Word64
  , gitRefLog :: RefLogKey HBS2Basic
  }
  deriving stock (Generic,Data)

data GitRepoHeadFact =
  GitRepoHeadFact1
  { gitRepoHeadRef   :: HashRef
  , gitRepoName      :: Text
  , gitRepoBrief     :: Text
  , gitRepoEncrypted :: Bool
  }
  deriving stock (Generic,Data)


data GitRepoHeadVersionFact =
  GitRepoHeadVersionFact1
  { gitRepoHeadVersion  :: Word64
  }
  deriving stock (Generic,Data)

data GitRepoFacts =
      GitRepoRefFact  GitRepoRefFact
    | GitRepoHeadFact HashRef GitRepoHeadFact
    | GitRepoHeadVersionFact HashRef GitRepoHeadVersionFact
    | GitRepoTxFact (LWWRefKey HBS2Basic) HashRef
    deriving stock (Generic,Data)


instance Serialise GitRepoRefFact
instance Serialise GitRepoHeadFact
instance Serialise GitRepoFacts
instance Serialise GitRepoHeadVersionFact

instance Pretty GitRepoFacts  where
  pretty (GitRepoRefFact x)    = pretty x
  pretty (GitRepoHeadFact ha x) = pretty ("gitrpoheadfact",ha,x)
  pretty (GitRepoHeadVersionFact ha x) = pretty ("gitrpoheadversionfact",ha,x)
  pretty (GitRepoTxFact r tx) = pretty ("gitrepotxfact", r, tx)

instance Pretty GitRepoRefFact where
  pretty (GitRepoFact1{..}) =
    parens ( "gitrepofact1" <+>  hsep [pretty gitLwwRef, pretty gitLwwSeq, pretty gitRefLog])

instance Pretty GitRepoHeadFact where
  pretty (GitRepoHeadFact1{..}) =
    parens ( "gitrepoheadfact1" <+>  hsep [pretty gitRepoHeadRef])

instance Pretty GitRepoHeadVersionFact where
  pretty (GitRepoHeadVersionFact1 v) = pretty v

