module HBS2.Net.IP.Addr (parseAddr, getHostPort, Pretty) where

import HBS2.Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text as Atto
import Data.Char
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Text qualified as Text
import Data.Text (Text)
import Network.Socket
import Network.SockAddr
import Prettyprinter

instance Pretty SockAddr where
  pretty sa = pretty (show sa)

getHostPort :: Text -> Maybe (String, PortNumber)
getHostPort s =  parseOnly p s & either (const Nothing) Just
  where
    p = do
      (h, p) <- pAddr
      pure (Text.unpack h, read (Text.unpack p))

parseAddr :: Text -> IO [AddrInfo]
parseAddr s = fromMaybe mempty <$> runMaybeT do
  (host,port) <- MaybeT $ pure $ parseOnly pAddr s & either (const Nothing) Just
  let hostS = Text.unpack host & Just
  let portS = Text.unpack port & Just
  MaybeT $ liftIO $ getAddrInfo (Just udp) hostS portS <&> Just

  where
    udp = defaultHints { addrSocketType = Datagram }

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


