{-# Language AllowAmbiguousTypes #-}
{-# Language RecordWildCards #-}
{-# Language MultiWayIf #-}
module NCQ3.EnduranceInProc where


import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff
import HBS2.Clock
import HBS2.Merkle
import HBS2.Polling
import HBS2.Peer.Proto.AnyRef

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.NCQ3
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Sweep
import HBS2.Storage.NCQ3.Internal

import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import NCQTestCommon

import Data.Generics.Labels
import Lens.Micro.Platform
import Network.ByteOrder qualified as N
import System.TimeIt
import Data.Fixed
import Data.HashSet qualified as HS
import Data.Either
import Data.HashPSQ qualified as HPSQ
import Data.HashMap.Strict qualified as HM
import Test.Tasty.HUnit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Ord
import Data.Set qualified as Set
import System.Random.MWC as MWC
import Control.Concurrent.STM qualified as STM
import Data.List qualified as List
import Control.Monad.Trans.Cont
import Control.Monad.Except
import System.IO.Temp qualified as Temp
import System.Environment (getExecutablePath)
import System.Process.Typed as PT
import System.IO qualified as IO
import System.IO.Error
import System.Posix.IO qualified as Posix
import GHC.IO.Handle qualified as GHC
import System.Random.Stateful
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import UnliftIO
import UnliftIO.IO.File
import UnliftIO.IO as IO
import UnliftIO.Directory

import Streaming.Prelude qualified as S

{-HLINT ignore "Functor law"-}


data EnduranceFSM =
    EnduranceIdle
  | EndurancePutBlk
  | EnduranceHasBlk
  | EnduranceGetBlk
  | EnduranceDelBlk
  | EndurancePutRef
  | EnduranceGetRef
  | EnduranceDelRef
  | EnduranceStorm
  | EnduranceCalm
  | EnduranceStop

buildCDF :: [(s, Double)] -> (V.Vector s, U.Vector Double)
buildCDF xs =
  let states = V.fromList (map fst xs)
      cdf    = U.fromList (scanl1 (+) (map snd xs))
  in (states, cdf)

-- выборка по бинарному поиску
sampleState :: MonadIO m => GenIO -> (V.Vector s, U.Vector Double) -> m s
sampleState g (states,cdf) = do
  let total = U.last cdf
  r <- liftIO $ uniformRM (0,total) g
  pure $ states V.! binarySearch cdf r

binarySearch :: U.Vector Double -> Double -> Int
binarySearch vec x = go 0 (U.length vec - 1)
  where
    go l r
      | l >= r    = l
      | otherwise =
          let mid = (l+r) `div` 2
          in if x <= vec U.! mid
               then go l mid
               else go (mid+1) r

-- | Pick a random key from a HashPSQ
getRandomFromPSQ :: forall k p v m . (MonadIO m, Hashable k, Ord k, Ord p)
                 => MWC.GenIO
                 -> TVar (HPSQ.HashPSQ k p v)
                 -> m (Maybe k)
getRandomFromPSQ g tvar = do
  psq <- readTVarIO tvar
  let n = HPSQ.size psq
  if n == 0
    then pure Nothing
    else do
      dropn <- liftIO $ uniformRM (0, n-1) g
      let e = fmap (view _1) . headMay $ drop dropn $ HPSQ.toList psq
      pure e


-- | Deleted = Left (), Alive = Right size
type BlockState = Either () Integer

-- | Deleted = Left (), Alive = Right destination
type RefState   = Either () HashRef

addHashRef :: forall m v . (MonadIO m) => GenIO  -> TVar (HashPSQ HashRef Double v) -> HashRef -> v -> m ()
addHashRef g what h v = do
  w <- liftIO $ uniformRM (0,1.0) g
  atomically do
    modifyTVar what (HPSQ.insert h w v)
    size <- readTVar what <&> HPSQ.size
    when (size > 100000 ) do
      modifyTVar what HPSQ.deleteMin


validateTestResult :: forall m . MonadUnliftIO m => FilePath -> m ()
validateTestResult logFile = do

  blocks <- newTVarIO (mempty :: HM.HashMap HashRef BlockState)
  refs   <- newTVarIO (mempty :: HM.HashMap HashRef RefState)

  let dict = makeDict @C do

        -- block-written: remember size
        entry $ bindMatch "block-written" $ nil_ \case
          [ HashLike h, LitIntVal n ] ->
            atomically $ modifyTVar blocks (HM.insert h (Right n))
          _ -> none

        -- block-deleted: mark deleted
        entry $ bindMatch "block-deleted" $ nil_ \case
          [ HashLike h ] ->
            atomically $ modifyTVar blocks (HM.insert h (Left ()))
          _ -> none

        -- has-block-result
        entry $ bindMatch "has-block-result" $ nil_ \case
          [ HashLike h, LitIntVal n ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right n0) | n0 == n -> none
              Just (Left ())             -> err $ red "has-block says present, but deleted" <+> pretty h
              _                          -> err $ red "has-block mismatch" <+> pretty h

          [ HashLike h ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Left ()) -> none
              Nothing        -> none
              Just (Right _) -> err $ red "has-block says missing, but we have" <+> pretty h
          _ -> none

        -- get-block-result
        entry $ bindMatch "get-block-result" $ nil_ \case
          [ HashLike h, HashLike _hx ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right _) -> none
              Just (Left ()) -> err $ red "get-block returned data for deleted block" <+> pretty h
              Nothing        -> err $ red "get-block returned data for unknown block" <+> pretty h

          [ HashLike h ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right _) -> err $ red "get-block missing, but expected present" <+> pretty h
              _              -> none
          _ -> none

        -- ref-updated
        entry $ bindMatch "ref-updated" $ nil_ \case
          [ HashLike h, HashLike hdest ] ->
            atomically $ modifyTVar refs (HM.insert h (Right hdest))
          _ -> none

        -- get-ref-result
        entry $ bindMatch "get-ref-result" $ nil_ \case
          [ HashLike h, HashLike hdest ] -> do
            really <- readTVarIO refs <&> HM.lookup h
            case really of
              Just (Right h0) | h0 == hdest -> none
              Just (Left ()) -> err $ red "get-ref returned value for deleted ref" <+> pretty h
              _ -> err $ red "get-ref mismatch" <+> pretty h <+> "got" <+> pretty hdest

          [ HashLike h ] -> do
            really <- readTVarIO refs <&> HM.lookup h
            case really of
              Just (Left ()) -> none
              Nothing        -> none
              Just (Right _) -> err $ red "get-ref says missing, but we have" <+> pretty h
          _ -> none

        -- ref-deleted
        entry $ bindMatch "ref-deleted" $ nil_ \case
          [ HashLike h ] ->
            atomically $ modifyTVar refs (HM.insert h (Left ()))
          _ -> none

  -- читаем лог построчно и скармливаем dict
  rs <- lines <$> liftIO (IO.readFile logFile)
  for_ rs $ \s -> case parseTop s of
    Left{}  -> none
    Right syn -> void $ run dict syn

  -- финальная статистика
  bs <- readTVarIO blocks
  rs' <- readTVarIO refs
  notice $ green "validate done"
         <+> "blocks:" <+> pretty (length [() | Right _ <- HM.elems bs])
         <+> "deleted-blocks:" <+> pretty (length [() | Left () <- HM.elems bs])
         <+> "refs:" <+> pretty (length [() | Right _ <- HM.elems rs'])
         <+> "deleted-refs:" <+> pretty (length [() | Left () <- HM.elems rs'])

ncq3EnduranceTestInProc :: forall m . MonadUnliftIO m => MakeDictM C m ()
ncq3EnduranceTestInProc = do

  entry $ bindMatch "test:ncq3:endurance:inproc" $ nil_ $ \syn -> do

    let dbl = \case
                 LitScientificVal x -> realToFrac x
                 LitIntVal x        -> realToFrac x
                 _ -> 0.00

    let int = \case
                 LitScientificVal x -> floor x
                 LitIntVal x        -> fromIntegral  x
                 _ -> 0

    wIdle     <-  dbl <$> lookupValueDef (mkDouble 200.00) "w:idle"
    wIdleDef  <-  dbl <$> lookupValueDef (mkDouble   0.25) "w:idle:def"
    wPutBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:putblk"
    wGetBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:getblk"
    wHasBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:hasblk"
    wDelBlk   <-  dbl <$> lookupValueDef (mkDouble   3.00) "w:delblk"
    wPutRef   <-  dbl <$> lookupValueDef (mkDouble   5.00) "w:putref"
    wGetRef   <-  dbl <$> lookupValueDef (mkDouble  10.00) "w:getref"
    wDelRef   <-  dbl <$> lookupValueDef (mkDouble   1.00) "w:delref"
    wStorm    <-  dbl <$> lookupValueDef (mkDouble   0.05) "w:storm"
    wCalm     <-  dbl <$> lookupValueDef (mkDouble   0.001) "w:calm"
    wNum      <-  int <$> lookupValueDef (mkInt 10000)     "w:num"
    wMaxBlk   <-  int <$> lookupValueDef (mkInt 262144)    "w:blk"
    wStormMin <-  dbl <$> lookupValueDef (mkDouble 1.00)   "w:stormmin"
    wStormMax <-  dbl <$> lookupValueDef (mkDouble 60.00)  "w:stormmax"

    runTest \TestEnv{..} -> do
      g <- liftIO $ MWC.createSystemRandom

      let (opts,args) = splitOpts [] syn

      let n = headDef wNum [ fromIntegral x | LitIntVal x <- args ]

      storms <- newTQueueIO

      rest   <- newTVarIO n
      blocks <- newTVarIO ( HPSQ.empty :: HPSQ.HashPSQ HashRef Double () )
      refs   <- newTVarIO ( HPSQ.empty :: HPSQ.HashPSQ HashRef Double HashRef )
      killed <- newTVarIO 0

      let getRandomBlock = liftIO $ getRandomFromPSQ g blocks
      let getRandomRef = liftIO $ getRandomFromPSQ g refs

      let actions = [ (EnduranceIdle,   wIdle)
                    , (EndurancePutBlk, wPutBlk)
                    , (EnduranceGetBlk, wGetBlk)
                    , (EnduranceHasBlk, wHasBlk)
                    , (EnduranceDelBlk, wDelBlk)
                    , (EndurancePutRef, wPutRef)
                    , (EnduranceGetRef, wGetRef)
                    , (EnduranceDelRef, wDelRef)
                    , (EnduranceStorm,  wStorm)
                    , (EnduranceCalm,   wCalm)
                    ]

      let dist = buildCDF actions   -- ← подготовили один раз

      fix \recover -> handle (\(e :: IOException) -> err (viaShow e) >> pause @'Seconds 1 >> recover) do

        flip runContT pure do

          let logFile = testEnvDir </> "op.log"

          let
            writeLog :: forall m1 . MonadIO m1 => Doc AnsiStyle -> m1 ()
            writeLog mess = liftIO (appendFile logFile (show $ mess <> line))

          ContT $ withAsync $ forever do
            join $ atomically (readTQueue storms)

          ContT $ withAsync $ forever do
            rest <- readTVarIO rest
            b    <- readTVarIO blocks <&> HPSQ.size
            r    <- readTVarIO refs <&> HPSQ.size
            k    <- readTVarIO killed

            notice $ green "status"
                       <+> "rest:" <+> pretty rest
                       <+> "b:" <+> pretty b
                       <+> "r:" <+> pretty r
                       <+> "k:" <+> pretty k

            pause @'Seconds 1

          let getNextState = sampleState g dist

          let defaultIdle = realToFrac wIdleDef :: Timeout 'Seconds

          idleTime  <- newTVarIO defaultIdle
          trelaxTill <- newTVarIO 0

          sto <- ContT $ ncqWithStorage testEnvDir

          flip fix EnduranceIdle \loop -> \case
            EnduranceIdle -> do
              readTVarIO idleTime >>= pause
              r <- readTVarIO rest
              if r <= 0 then loop EnduranceStop else getNextState >>= loop

            EndurancePutBlk -> do
              bsize <- liftIO $ uniformRM (1, wMaxBlk) g
              bs <- LBS.fromStrict <$> liftIO (genRandomBS g bsize)
              h <- liftIO $ putBlock sto bs `orDie` "can't write block"
              let mess = "block-written" <+> pretty h <+> pretty (LBS.length bs)
              addHashRef g blocks (coerce h) ()
              debug mess
              writeLog mess
              atomically $ modifyTVar rest pred
              getNextState >>= loop

            EnduranceDelBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h -> do
                liftIO $ delBlock sto (coerce h)
                let mess = "block-deleted" <+> pretty h
                debug mess
                writeLog mess

              getNextState >>= loop

            EnduranceHasBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h -> do
                f <- lift $ hasBlock sto (coerce h)
                let mess = "has-block-result" <+> pretty h <+> pretty f
                debug mess
                writeLog mess

              getNextState >>= loop

            EnduranceGetBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h ->  do
                mbs <- lift $ getBlock sto (coerce h)

                let mess = case mbs of
                      Just bs -> "get-block-result" <+> pretty h <+> pretty (hashObject @HbSync bs)
                      Nothing -> "get-block-result" <+> pretty h

                debug mess
                writeLog mess

              getNextState >>= loop

            EndurancePutRef -> do
              href <- liftIO (genRandomBS g 32) <&> HashRef . coerce
              blk  <- getRandomBlock
              for_ blk $ \val -> do
                lift $ updateRef sto (RefAlias2 mempty href) (coerce val)
                addHashRef g refs href (HashRef $ hashObject @HbSync val)
                let mess = "ref-updated" <+> pretty href <+> pretty val
                debug mess
                writeLog mess

              atomically $ modifyTVar rest pred
              getNextState >>= loop

            EnduranceGetRef -> do
              e <- getRandomRef
              for_ e $ \h -> do
                what <- lift $ getRef sto (RefAlias2 mempty h)
                let mess = "get-ref-result" <+> pretty h <+> pretty what
                debug mess
                writeLog mess

              getNextState >>= loop

            EnduranceDelRef -> do
              e <- getRandomRef
              for_ e $ \h -> do
                lift $ delRef sto (RefAlias2 mempty h)
                let mess = "ref-deleted" <+> pretty h
                debug mess
                writeLog mess

              getNextState >>= loop

            EnduranceStop -> do
              notice $ green "done"
              notice $ "validate" <+> pretty logFile
              liftIO $ validateTestResult logFile

            EnduranceCalm -> do
              n <- liftIO $ uniformRM (0.5,10.00) g
              debug $ "CALM" <+> pretty n
              pause @'Seconds (realToFrac n)
              getNextState >>= loop

            EnduranceStorm -> do
              now <- getTimeCoarse
              relaxTill <- readTVarIO trelaxTill
              itn <- readTVarIO idleTime
              if | itn < defaultIdle -> loop EnduranceIdle
                 | now < relaxTill   -> loop EnduranceIdle
                 | otherwise -> do
                    t0 <- liftIO $ uniformRM (wStormMin,wStormMax) g
                    debug $ red "FIRE IN DA HOLE!" <+> pretty t0
                    atomically $ writeTQueue storms do
                      atomically $ writeTVar idleTime 0
                      pause @'Seconds (realToFrac t0)
                      atomically $ writeTVar idleTime defaultIdle
                      t1 <- getTimeCoarse
                      atomically $ writeTVar trelaxTill (t1 + ceiling 10e9)
                    getNextState >>= loop


