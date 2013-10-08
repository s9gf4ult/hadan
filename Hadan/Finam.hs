{-# LANGUAGE
  QuasiQuotes
, FlexibleContexts
  #-}
module Hadan.Finam where


import Codec.Text.IConv
import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Internal (ResumableSource(..))
import Data.Conduit.Text
import Data.Time
import Hadan.Data.Candle
import Hadan.Data.Parsers.AlorTrade
import Network.HTTP.Conduit
import Network.URL
import System.IO (stderr, hPutStrLn)
import System.Locale
import Text.HTML.DOM
import Text.XML (Document)
import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Scraping
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Util as U
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL



showGraphics :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                => Manager -> m Document
showGraphics manager = do
  req <- liftIO $ parseUrl "http://www.finam.ru"
  resp <- http req manager
  lbs <- responseBody resp $$+- sinkLbs
  let convlbs = convert "CP1251" "UTF-8" lbs
  return $ parseLBS convlbs
