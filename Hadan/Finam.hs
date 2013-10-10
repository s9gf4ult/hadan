{-# LANGUAGE
  QuasiQuotes
, FlexibleContexts
, OverloadedStrings
  #-}
module Hadan.Finam where


import Codec.Text.IConv
import Control.Applicative
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
import Text.XML (Document, Node(..), Element(..))
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Util as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

-- import Debug.Trace

downloadDoc :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                => Manager -> String -> m Document
downloadDoc manager url = do
  req <- liftIO $ parseUrl url
  resp <- http req manager
  lbs <- responseBody resp $$+- sinkLbs
  let convlbs = convert "CP1251" "UTF-8" lbs
  return $ parseLBS convlbs

fixUrlHost :: Host -> URL -> URL
fixUrlHost host url@(Url {url_type = ut}) = case ut of
  HostRelative -> url { url_type = Absolute host}
  _ -> url

mutHost :: (URL -> URL) -> String -> String
mutHost mut s = case importURL s of
  Nothing -> s
  Just u -> exportURL $ mut u

nthChild :: Int -> Axis
nthChild n = check isnth
  where
    isnth cur = (n-1) == (length $ precedingSibling cur)

-- | .sect-menu > table:nth-child(1) > tr:nth-child(1) > td:nth-child(5) > a:nth-child(1)
graphicsAxis :: Axis
graphicsAxis = descendant >=> attributeIs "class" "sect-menu"
           >=> child >=> element "table"
           >=> child >=> element "tr"
           >=> child >=> element "td"
           >=> child >=> element "a" >=> findByContent "Графики"

findByContent :: T.Text -> Axis
findByContent t = check getCont
  where
    getCont cur = case (child >=> content) cur of
      [a] -> a == t
      _ -> False


-- | tr.last:nth-child(13) > td:nth-child(1) > a:nth-child(1)
exportsAxis :: Axis
exportsAxis = descendant >=> element "tr" >=> attributeIs "class" "last"
                >=> child >=> element "td"
                >=> child >=> element "a" >=> findByContent "Экспорт котировок"

getAHref :: Cursor -> T.Text
getAHref cur = case node cur of
  (NodeElement (Element _ attrs _)) ->
    case M.lookup "href" attrs of
      Nothing -> error "'href' attribute not found"
      Just r -> r
  _ -> error "Not a NodeElement"


getLinkByAxis :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                       => Manager -> Axis -> String -> m T.Text
getLinkByAxis man axis url = do
  doc <- downloadDoc man url
  let c = axis $ fromDocument doc
  case c of
    [a] -> return $ getAHref a
    _ -> fail "could not find element or found too much"


downloadFollower :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                       => Manager -> m T.Text
downloadFollower man = do
  gr <- mutHost (fixUrlHost host) <$> getLinkByAxis man graphicsAxis "http://www.finam.ru"
  mutHost (fixUrlHost host) <$> getLinkByAxis man exportsAxis gr
  where
    host = Host (HTTP False) "www.finam.ru" Nothing