{-# Language  TemplateHaskell #-}
{-# Language  DeriveFunctor #-}
module HBS2.Merkle where

import HBS2.Prelude
import HBS2.Hash

import Codec.Serialise (serialise, deserialiseOrFail)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Data
import Data.Foldable (forM_, traverse_)
import Data.List qualified as List
import Data.Text (Text)
import GHC.Generics
import Lens.Micro.Platform
import Prettyprinter

newtype MerkleHash = MerkleHash { fromMerkleHash :: Hash HbSync }
                     deriving newtype (Eq,Ord,IsString,Pretty)
                     deriving stock (Data,Generic)

class Foldable t => Chunks t a where
  chunks :: Int -> a -> t a

instance Chunks [] ByteString where
  chunks i bs = go [] bs
    where
      size = fromIntegral i
      go acc x | BS.null x  = acc
               | BS.length x <= size = acc <> [x]
               | otherwise = go (acc <> [BS.take size x]) (BS.drop size x)

instance Chunks [] LBS.ByteString where
  chunks i bs = go [] bs
    where
      size = fromIntegral i
      go acc x | LBS.null x  = acc
               | LBS.length x <= size = acc <> [x]
               | otherwise = go (acc <> [LBS.take size x]) (LBS.drop size x)

instance Chunks [] [a] where
  chunks i xs = go xs
    where
      go [] = []
      go es | length es <= i = [es]
            | otherwise = let (p, ps) = List.splitAt i es in p : go ps


data PTree  a = T [PTree a] | L a
                deriving stock (Show, Functor, Generic)


instance Foldable PTree where
  foldr fn acc (L a)  = fn a acc
  foldr fn acc (T xs) = go acc xs
    where
      go b [] = b
      go b (y:ys) = foldr fn (go b ys) y

instance Traversable PTree where
  traverse fn (L a) = L <$> fn a
  traverse fn (T xs) = T <$> traverse (traverse fn) xs

newtype MaxNum a = MaxNum a
newtype MaxSize a = MaxSize a


newtype MNodeData =
  MNodeData
  { _mnodeHeight :: Integer
  }
  deriving stock (Generic,Data,Show)

makeLenses ''MNodeData

instance Serialise MNodeData

data AnnMetaData = NoMetaData | ShortMetadata Text | AnnHashRef (Hash HbSync)
  deriving stock (Generic,Data,Show,Eq)

instance Serialise AnnMetaData

data MTreeAnn a = MTreeAnn
 { _mtaMeta :: !AnnMetaData
 , _mtaCrypt :: !MTreeEncryption
 , _mtaTree :: !(MTree a)
 }
  deriving stock (Generic,Data,Show)

instance Serialise a => Serialise (MTreeAnn a)

data MerkleEncryptionType
  deriving stock (Data)

data MTreeEncryption
  = NullEncryption
  | CryptAccessKeyNaClAsymm (Hash HbSync)
  deriving stock (Generic,Data,Show)

instance Serialise MTreeEncryption

data MTree a = MNode MNodeData [Hash HbSync] | MLeaf a
               deriving stock (Generic,Data,Show)

instance Serialise a => Serialise (MTree a)

newMNode :: Integer -> [Hash HbSync] -> MTree a
newMNode h = MNode (MNodeData h)

toPTree :: (Chunks [] a)
        => MaxSize Int
        -> MaxNum Int
        -> a
        -> PTree a

toPTree (MaxSize s) (MaxNum n) items | n <= 1 =
  case T $ fmap L (chunks s items) of
    T [L x] -> L x
    _       -> T []

toPTree (MaxSize s) (MaxNum n) items = go $ T (fmap L (chunks s items))
  where
    go (T []) = T []
    go (T [L x]) = L x
    go (T xs) | length xs <= n = T xs
              | otherwise = go $ T $ fmap go [ T x | x <- chunks n xs ]

    go leaf = leaf


makeMerkle :: (Monad m, Serialise a, Serialise (MTree a))
           => Integer -- | initial height
           -> PTree a
           -> ((Hash HbSync, MTree a, LBS.ByteString) -> m ())
           -> m (Hash HbSync)

makeMerkle h0 pt f = fst <$> go h0 pt
  where
    go hx (T xs) = do
      rs <- mapM (go hx) xs
      let hxx = maximumDef hx (fmap snd rs)
      let o = newMNode hxx (fmap fst rs)
      let bs = serialise o
      let h  = hashObject bs
      f (h, o, bs)
      pure (h, 1+hxx)

    go hx (L x) = do
      let o = MLeaf x
      let bs = serialise o
      let h  = hashObject bs
      f (h, o, bs)
      pure (h, 1+hx)



walkMerkle' :: (Serialise (MTree a), Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) (MTree a) -> m () )
           -> m ()

walkMerkle' root flookup sink = go root
  where
    go hash = do
      -- t <- (either (error . show) id . deserialiseOrFail <$>) <$> flookup hash
      t <- ((either (const Nothing) Just . deserialiseOrFail) =<<) <$> flookup hash
      case t of
        Just n@(MLeaf _) -> sink (Right n)
        Just n@(MNode _ hashes) -> sink (Right n) >> traverse_ go hashes
        Nothing -> sink (Left hash)

walkMerkle :: (Serialise (MTree a), Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) a -> m () )
           -> m ()

walkMerkle root flookup sink = walkMerkle' root flookup withTree
  where
    withTree = \case
        (Right (MLeaf s))   -> sink (Right s)
        (Right (MNode _ _)) -> pure ()
        Left hx             -> sink (Left hx)


walkMerkleTree :: (Serialise (MTree a), Monad m)
           => MTree a
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) a -> m () )
           -> m ()

walkMerkleTree tree flookup sink = case tree of
    (MLeaf s)   -> sink (Right s)
    (MNode _ hashes) -> forM_ hashes \h -> walkMerkle h flookup sink

