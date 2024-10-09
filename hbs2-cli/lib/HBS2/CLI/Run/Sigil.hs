{-# OPTIONS_GHC -Wno-name-shadowing #-}
module HBS2.CLI.Run.Sigil where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Storage
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil

import Data.List qualified as L
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Lens.Micro.Platform


sigilEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m, HasStorage m)
             => MakeDictM c m ()
sigilEntries = do

  entry $ bindMatch "hbs2:sigil:sign-pubkey" $ \case
    [ ListVal (SymbolVal sigil : (hasKey "sign-pubkey" -> Just s)) ] -> do
      pure s

    _ -> throwIO $ BadFormException @C nil


  entry $ bindMatch "hbs2:sigil:encrypt-pubkey" $ \case
    [ ListVal (SymbolVal sigil : (hasKey "encrypt-pubkey" -> Just s)) ] -> do
      pure s

    _ -> throwIO $ BadFormException @C nil

  brief "parses sigil"
    $ args [ arg "sigil" "string" ]
    $ examples [qc|hbs2:sigil:parse [str:read-file some.sigil]|]
    $ entry $ bindMatch "hbs2:sigil:parse" $ \case
      [StringLike s] -> do

        let bs = BS8.pack s
        sigil <- pure (parseSerialisableFromBase58 @(Sigil 'HBS2Basic) bs)
                    `orDie` "parse sigil failed"

        (_,sd) <- pure (unboxSignedBox0 @(SigilData 'HBS2Basic) (sigilData sigil))
                    `orDie` "signature check failed"

        pure (parseTop $ show $ parens ("sigil" <> line <> indent 2 (vcat $ [pretty sigil, pretty sd])))
                `orDie` "bad sigil"
                <&> head

      _ -> throwIO $ BadFormException @C nil


  brief "loads sigil from hbs2 store as base58 string"
    $ args [arg "hash" "string" ]
    $ returns  "sigil" "string"
    $ entry $ bindMatch "hbs2:sigil:load:base58" $ \case
      [HashLike key] -> lift do
        sto <- getStorage
        r <- loadSigil @HBS2Basic sto key >>= orThrowUser "no sigil found"
        pure $ mkStr @c ( show $ pretty $ AsBase58 r  )

      _ -> throwIO $ BadFormException @c nil

  brief "stores sigil to hbs2 store"
    $ args [arg "string" "file" ]
    $ returns "string" "hash"
    $ entry $ bindMatch "hbs2:sigil:store:file" $ \case
      [StringLike fn] -> lift do
        sto <- getStorage
        lbs <- liftIO (LBS.readFile fn)
        sigil <- decodeSigil @HBS2Basic lbs & orThrowUser "invalid sigil file"
        href <- storeSigil sto sigil
        pure $ mkStr ( show $ pretty href )

      _ -> throwIO $ BadFormException @c nil


  entry $ bindMatch "hbs2:sigil:create:from-keyring" $ \syn -> do

    args <- case syn of
      [ StringLike s ] -> pure (fmap snd . headMay, s)
      [ StringLike p, StringLike s ] -> pure ( findKey p, s)
      [ LitIntVal n, StringLike s ] -> pure ( L.lookup n, s)

      _ -> throwIO $ BadFormException @C nil

    let lbs = BS8.pack (snd args)

    cred <- pure (parseCredentials @'HBS2Basic (AsCredFile lbs))
               `orDie` "bad keyring data"

    let es = zip [0..]
                 [ p | KeyringEntry p _ _
                     <- view peerKeyring cred
                 ]

    enc <- pure (fst args es)
           `orDie` "key not found"

    sigil <- pure (makeSigilFromCredentials @'HBS2Basic cred enc Nothing Nothing)
              `orDie` "can't create a sigil"

    pure $ mkStr (show $ pretty $ AsBase58 sigil)

  where
    findKey s xs = headMay [ k
                           | e@(_,k) <- xs
                           , L.isPrefixOf s (show $ pretty (AsBase58 k))
                           ]


