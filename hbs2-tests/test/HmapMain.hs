{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Main where

import Data.Typeable
import Data.Dynamic
import Data.Proxy
import Data.Kind

import Prettyprinter


data Key = forall a . (Unfuck a, Eq a) => Key (Proxy a) Dynamic

class Typeable a => Unfuck a where
  unfuck :: Proxy a -> Dynamic -> Maybe a

instance Typeable a => Unfuck a where
  unfuck _ = fromDynamic @a

newKey :: forall a . (Eq a, Typeable a, Unfuck a) => a -> Key
newKey s = Key (Proxy @a) (toDyn s)

instance Eq Key where
  (==) (Key p1 a) (Key p2 b) = unfuck p1 a == unfuck p1 b

main :: IO ()
main = do
  let k1 = newKey 22
  let k2 = newKey 33
  let k3 = newKey "JOPA"

  print $ "k1 == k1:" <+> pretty (k1 == k1)
  print $ "k2 == k2:" <+> pretty (k2 == k2)
  print $ "k1 == k2:" <+> pretty (k1 == k2)
  print $ "k3 == k3:" <+> pretty (k3 == k3)
  print $ "k3 == k2:" <+> pretty (k3 == k2)
  print $ "k3 == k1:" <+> pretty (k3 == k1)


  -- _ <- race ( pause ( 60 :: Timeout 'Seconds) ) $ forever $ do
  --       let gen = arbitrary @MyKey
  --       let n = 100
  --       keys <- replicateM 10 (sample' @MyKey gen) <&> mconcat
  --       vals <- replicateM 100 (randomIO @Int)

  --       let kv = zip keys vals

  --       forM_ kv $ \(k,v) -> do

  --         m <- readTVarIO tm

  --         let z = withKey k id

  --         undefined

          -- atomically $ writeTVar tm z
          -- atomically $ modifyTVar km (k:)

          -- kl <- readTVarIO km

          -- when (length kl > 1000) $ do
          --   let (a,b) = L.splitAt 1000 kl

          --   m1 <- readTVarIO tm
          --   forM_ b $ \z3 -> do
          --     let m2 = withKey z3 $ \z3 -> delete z3 m1
          --     pure ()

          --   atomically $ writeTVar km b

          --   pure ()



  pure ()

