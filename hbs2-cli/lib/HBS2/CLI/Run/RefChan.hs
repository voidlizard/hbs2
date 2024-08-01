module HBS2.CLI.Run.RefChan where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan

import HBS2.Peer.Proto.LWWRef
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Control.Monad.Trans.Cont

import Text.InterpolatedString.Perl6 (qc)

refchanEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM  c m ()
refchanEntries = do

  brief "requests all rechans that peer is subcribed to"
    $ args []
    $ returns "list" "list of all refchans"
    $ examples [qc|

(hbs2:refchan:list)
("Atg67E6CPMJWKvR9BvwZTTEjg3Hjz4CYCaEARGANepGP"
 "A5W6jPBjzvdpxaQ2e8xBLYaRZjPXzi4yX7xjC52gTiKk"
 "EjjK7rpgRRJ4yzAhTcwis4XawwagCbmkns8n73ogY3uS")
    |]
    $ entry $ bindMatch "hbs2:refchan:list" $ \case
        [] -> do
          flip runContT pure do
            so <- detectRPC `orDie` "rpc not found"
            api <- ContT $ withRPC2 @PeerAPI @UNIX so
            r <- callService @RpcPollList2 api (Just "refchan", Nothing)
                   >>= orThrowUser "can't get refchan list"
            pure $ mkList $ fmap (mkStr . show . pretty . AsBase58 . view _1) r

        _ -> throwIO (BadFormException @C nil)


