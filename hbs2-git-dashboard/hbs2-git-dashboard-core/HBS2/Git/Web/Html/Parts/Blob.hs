module HBS2.Git.Web.Html.Parts.Blob where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Types

import HBS2.Git.Web.Html.Markdown

import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)

import Skylighting qualified as Sky
import Skylighting.Tokenizer
import Skylighting.Format.HTML.Lucid as Lucid

import Control.Applicative

{-HLINT ignore "Functor law"-}


doRenderBlob :: (MonadReader DashBoardEnv m, MonadUnliftIO m)
             =>  (Text -> HtmlT m ())
             -> LWWRefKey HBS2Basic
             -> BlobInfo
             -> HtmlT m ()

doRenderBlob fallback = doRenderBlob' fallback id

doRenderBlob' :: (MonadReader DashBoardEnv m, MonadUnliftIO m)
             =>  (Text -> HtmlT m ())
             -> (Text -> Text)
             -> LWWRefKey HBS2Basic
             -> BlobInfo
             -> HtmlT m ()

doRenderBlob' fallback preprocess lww BlobInfo{..} = do
  fromMaybe mempty <$> runMaybeT do

    guard (blobSize < 10485760)

    let fn = blobName & coerce
    let syntaxMap = Sky.defaultSyntaxMap

    syn <- ( Sky.syntaxesByFilename syntaxMap fn
               & headMay
           ) <|> Sky.syntaxByName syntaxMap "default"
           & toMPlus

    lift do

      txt <- lift (readBlob lww blobHash)
                <&> LBS.toStrict
                <&> Text.decodeUtf8

      case blobSyn of
        BlobSyn (Just "markdown") -> do

          div_ [class_ "lim-text"] do
            toHtmlRaw (renderMarkdown' txt)

        _ -> do

          txt <- lift (readBlob lww blobHash)
                    <&> LBS.toStrict
                    <&> Text.decodeUtf8
                    <&> preprocess

          let config = TokenizerConfig { traceOutput = False, syntaxMap = syntaxMap }

          case tokenize config syn txt of
            Left _ -> fallback txt
            Right tokens -> do
              let fo = Sky.defaultFormatOpts { Sky.numberLines = False, Sky.ansiColorLevel = Sky.ANSI256Color  }
              let code = renderText (Lucid.formatHtmlBlock fo tokens)
              toHtmlRaw code


