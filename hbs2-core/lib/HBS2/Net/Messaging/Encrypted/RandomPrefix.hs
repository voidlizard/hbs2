{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilyDependencies #-}
module HBS2.Net.Messaging.Encrypted.RandomPrefix
  ( module Data.ByteString.Builder
  , runCodeLazy
  , RandomPrefix(..)
  , PrefixMethod1(..)
  ) where

import Data.Word
import Data.Bits
-- import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.ByteString.Builder
import Data.Maybe
import Lens.Micro.Platform
import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Safe
import Data.List.Split (chunksOf)
import System.Random hiding (next)

data NOP
data LOADB
data SKIPBI
data ANDBI
data ORBI
data XORBI
data ADDBI
data SUBBI
data MULTBI
data REPEAT
data RET

class Emittable a where
  emit :: a -> Builder

class (Emittable (Arg a), KnownNat (Opcode a)) => Instruction a where
  type family Opcode a = (code :: Nat) | code -> a
  type family Arg a :: Type

data OP = forall a . (Instruction a, Emittable (Proxy a)) =>
  OP (Proxy a) (Arg a) | BYTE Word8

instance Instruction a => Emittable (Proxy a) where
  emit _ = word8 . fromIntegral $ natVal (Proxy @(Opcode a))

instance Emittable OP where
  emit (OP o arg) = emit o <> emit arg
  emit (BYTE w) = word8 w

instance Emittable () where
  emit = mempty

instance Emittable Word8 where
  emit = word8

instance Emittable b => Emittable [b]  where
  emit xs= mconcat (fmap emit xs)

instance Instruction NOP where
  type instance Opcode NOP = 0xFE
  type instance Arg NOP = ()

instance Instruction LOADB where
  type instance Opcode LOADB = 0x01
  type instance Arg LOADB = Word8

instance Instruction SKIPBI where
  type instance Opcode SKIPBI = 0x02
  type instance Arg SKIPBI = Word8

instance Instruction ORBI where
  type instance Opcode ORBI= 0x03
  type instance Arg ORBI = Word8

instance Instruction ANDBI where
  type instance Opcode ANDBI= 0x04
  type instance Arg ANDBI = Word8

instance Instruction XORBI where
  type instance Opcode XORBI= 0x05
  type instance Arg XORBI = Word8

instance Instruction ADDBI where
  type instance Opcode ADDBI = 0x06
  type instance Arg ADDBI = Word8

instance Instruction SUBBI where
  type instance Opcode SUBBI = 0x07
  type instance Arg SUBBI = Word8

instance Instruction MULTBI where
  type instance Opcode MULTBI = 0x08
  type instance Arg MULTBI = Word8

instance Instruction REPEAT where
  type instance Opcode REPEAT = 0xC0
  type instance Arg REPEAT = Word8

instance Instruction RET where
  type instance Opcode RET = 0x00
  type instance Arg RET = ()

op :: forall a . Instruction a
   => Arg a
   -> OP

op = OP (Proxy @a)

byte :: Word8 -> OP
byte = BYTE

runCodeLazy :: ByteString -> (Maybe Word8, ByteString)
runCodeLazy s = runState (execStateT (runMaybeT (go s)) Nothing) s
  where

    next = MaybeT . pure . LBS.uncons

    update rest = do
      lift $ lift $ put rest
      pure rest

    go bs = do
      r <- next bs
      void $ update (snd r)
      exec r >>= update
             >>= go

    exec (b, rest)
      | b == code @NOP    = nop rest
      | b == code @LOADB  = loadb rest
      | b == code @SKIPBI = skipbi rest
      | b == code @ORBI   = orbi rest
      | b == code @ANDBI  = andbi rest
      | b == code @XORBI  = xorbi rest
      | b == code @ADDBI  = addbi rest
      | b == code @SUBBI  = subbi rest
      | b == code @MULTBI = multi rest
      | b == code @RET    = ret rest
      -- | b == code @REPEAT = repeatN rest -- dangerous
      | otherwise         = nop rest

    ret _ = mzero -- pure

    nop = pure

    multi bs = do
      (n, rest) <- next bs
      apply (*) n
      pure rest

    addbi bs = do
      (n, rest) <- next bs
      apply (+) n
      pure rest

    subbi bs = do
      (n, rest) <- next bs
      apply (-) n
      pure rest

    orbi bs = do
      (n, rest) <- next bs
      apply (.|.) n
      pure rest

    andbi bs = do
      (n, rest) <- next bs
      apply (.&.) n
      pure rest

    xorbi bs = do
      (n, rest) <- next bs
      apply xor n
      pure rest

    skipbi bs = do
      (n, rest) <- next bs
      let r2 = LBS.drop (fromIntegral n) rest
      update r2
      pure r2

    loadb bs = do
      (n, rest) <- next bs
      put (Just n)
      pure rest

    _repeatN bs = do
      (n, rest) <- next bs

      rest' <- replicateM (min 16 (fromIntegral n)) $ do
                next rest >>= exec

      pure (headDef "" rest')

    apply fn n = do
      st <- get
      put $ Just $ fromMaybe 0 st `fn` fromIntegral n

    code :: forall a b . (Integral b, Instruction a) => b
    code = fromIntegral (natVal (Proxy @(Opcode a)))


class (Monad m) => RandomPrefix a m where
  randomPrefix :: a -> m Builder

data PrefixMethod1 = PrefixMethod1 Int Word8 Word8

partsMethod1 :: Int -> Word8 -> Word8 -> [Word8]
partsMethod1 k b n = nums
  where
    (d, r) = n `divMod` b
    nums = r : replicate (fromIntegral d) b & chunksOf k & fmap sum

instance MonadIO m => RandomPrefix PrefixMethod1 m where

  randomPrefix (PrefixMethod1 k a x) = liftIO do
    let nums = partsMethod1 k a x
    me <- liftIO $ replicateM (length nums) $ randomRIO (0,2 :: Integer)
    opcodes <- forM (zip me nums) $ \z@(_, n) ->
      case fst z of
        1 -> do
          let (w,p) = n `divMod` 2
          pure $ op @ADDBI p : replicate 2 (op @ADDBI w)

        2 -> do
          j <- randomIO @Word8
          pure [ op @SUBBI j, op @ADDBI (n+j) ]

        _ -> pure [ op @ADDBI n ]

    sn <- randomRIO (1,6)
    bytes <- replicateM sn (randomIO @Word8) <&> fmap byte
    let fin = op @SKIPBI (fromIntegral sn) : bytes

    pure $ emit $ mconcat opcodes <> fin <> [ op @RET () ]

