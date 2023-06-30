{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Net.IP.Addr
  ( parseAddrUDP
  , parseAddrTCP
  , getHostPort
  , Pretty
  , IPAddrPort(..)
  , AddrPriority(..)
  ) where

import HBS2.Prelude.Plated

import Codec.Serialise (Serialise(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text as Atto
import Data.Char
import Data.Function
import Data.Functor
import Data.IP
import Data.Maybe
import Data.Text qualified as Text
import Network.Socket
import Data.Word (Word16)

class AddrPriority a where
  addrPriority :: a -> Int

instance AddrPriority SockAddr where
  addrPriority = \case
    SockAddrInet{}  -> 1
    SockAddrInet6{} -> 2
    SockAddrUnix{}  -> 3

instance Pretty SockAddr where
  pretty sa = pretty (show sa)

instance Serialise IP
instance Serialise IPv4
instance Serialise IPv6

newtype IPAddrPort e =
  IPAddrPort (IP, Word16)
  deriving stock (Generic,Eq,Ord,Show)

instance Hashable IPv4
instance Hashable IPv6
instance Hashable IP
instance Hashable (IPAddrPort e)

instance Serialise (IPAddrPort e)

instance Pretty IP where
  pretty ip = case ip of
              i4@(IPv4{}) -> pretty (show i4)
              i6@(IPv6{}) -> brackets $ pretty (show i6)

instance Pretty (IPAddrPort e) where
  pretty (IPAddrPort (ip,p)) = pretty (show pip) <> colon <> pretty p
    where
      pip = pretty ip

instance IsString (IPAddrPort e) where
  fromString s = IPAddrPort (read h, fromIntegral p)
    where
      (h,p) = fromMaybe (error $ "no parse IPAddrPort: " <> show s)
                (getHostPort (Text.pack s))

instance FromStringMaybe (IPAddrPort e) where
  fromStringMay x = IPAddrPort <$> ( (,) <$> ip <*> fmap fromIntegral po)
    where
      hp = getHostPort (Text.pack x)
      ip = readMay . fst =<< hp
      po = snd <$> hp

getHostPort :: Text -> Maybe (String, PortNumber)
getHostPort s =  parseOnly p s & either (const Nothing) Just
  where
    p = do
      (h, p) <- pAddr <|> tcppAddr
      pure (Text.unpack h, read (Text.unpack p))


parseAddrUDP ::  Text -> IO [AddrInfo]
parseAddrUDP = parseAddr Datagram

parseAddrTCP ::  Text -> IO [AddrInfo]
parseAddrTCP = parseAddr Stream

parseAddr :: SocketType -> Text -> IO [AddrInfo]
parseAddr tp s = fromMaybe mempty <$> runMaybeT do
  (host,port) <- MaybeT $ pure $ parseOnly pAddr s & either (const Nothing) Just
  let hostS = Text.unpack host & Just
  let portS = Text.unpack port & Just
  MaybeT $ liftIO $ getAddrInfo (Just udp) hostS portS <&> Just

  where
    udp = defaultHints { addrSocketType = tp }

tcppAddr :: Parser (Text, Text)
tcppAddr = "tcp://" *> pAddr

pAddr :: Parser (Text, Text)
pAddr = pIP6 <|> pIP4 <|> pHostName

pIP6 :: Parser (Text, Text)
pIP6 = do
  skipSpace

  hostAddr <- do
    void $ char '['
    p <- Atto.takeWhile ( \c -> isHexDigit c || c == ':' )
    void $ char ']'
    pure p

  port <- do
    void $ char ':'
    Atto.takeWhile isDigit

  skipSpace
  endOfInput

  pure (hostAddr, port)

pIP4 :: Parser (Text, Text)
pIP4 = do
  skipSpace

  hostAddr0 <- replicateM 3 $ do
    n <- Atto.takeWhile isDigit
    dot <- string "."
    pure ( n <> dot )

  hostAddr1 <- Atto.takeWhile isDigit

  port <- do
    void $ char ':'
    Atto.takeWhile isDigit

  skipSpace
  endOfInput

  pure (mconcat hostAddr0 <> hostAddr1, port)

pHostName :: Parser (Text, Text)
pHostName = do
  skipSpace
  host' <- Atto.takeWhile (/= ':')
  void $ char ':'
  port <- decimal
  let host = if Text.null host' then "localhost" else host'
  pure (host, Text.pack (show port))


