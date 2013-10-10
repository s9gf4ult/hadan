{-# LANGUAGE
  QuasiQuotes
, FlexibleContexts
, OverloadedStrings
  #-}
module Hadan.Finam where

import Text.Printf
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
import Text.XML (Document, Node(..), Element(..), Name(..))
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
fixUrlHost host url@(URL {url_type = ut}) = case ut of
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

-- | form#chartform
chartformAxis :: Axis
chartformAxis = descendant >=> element "form" >=> attributeIs "id" "chartform"

getAttr :: Cursor -> Name -> T.Text
getAttr cur name = case node cur of
  (NodeElement (Element _ attrs _)) ->
    case M.lookup name attrs of
      Nothing -> error $ show name ++ " attribute not found"
      Just r -> r
  _ -> error "Not a NodeElement"


getAttrByAxis :: Name -> Axis -> Document -> T.Text
getAttrByAxis attr axis doc = case axis $ fromDocument doc of
  [cur] -> getAttr cur attr
  x     -> error $ "found " ++ (show $ length x) ++ " elements, but must be 1"

downloadFollower :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                    => Manager
                    -> m (T.Text, T.Text) -- ^ Follower link and form action
downloadFollower man = do
  gr <- mutHost (fixUrlHost host)
        . T.unpack
        . getAttrByAxis "href" graphicsAxis
        <$> downloadDoc man "http://www.finam.ru"
  doc <- downloadDoc man gr
  let follower = T.pack
                 $ mutHost (fixUrlHost host)
                 $ T.unpack $ getAttrByAxis "href" exportsAxis doc
      action = getAttrByAxis "action" chartformAxis doc
  return (follower, action)
  where
    host = Host (HTTP False) "www.finam.ru" Nothing

tickParams :: Day -> Day -> String -> [(String, String)]
tickParams from to stock = [("market", "1")
                           ,("em", "3")
                           ,("code", stock)
                           ,("df", show df)
                           ,("mf", show mf)
                           ,("yf", show yf)
                           ,("dt", show dt)
                           ,("mt", show mt)
                           ,("yt", show yt)
                           ,("p", "2")
                           ,("f", fname)
                           ,("e", ".txt")
                           ,("cn", stock)
                           ,("dtf", "1")
                           ,("tmf", "1")
                           ,("MSOR", "0")
                           ,("mstime", "on")
                           ,("mstimever", "1")
                           ,("sep", "1")
                           ,("sep2", "2")
                           ,("daft", "1")]
  where
    (yf, mf, df) = toGregorian from
    (yt, mt, dt) = toGregorian to
    fname = printf "%s_%s_%s" stock (fmtdt yf mf df) (fmtdt yt mt dt)
    fmtdt :: Integer -> Int -> Int -> String
    fmtdt y m d = printf "%02d%02d%02d" (if y >= 2000 then y - 2000 else y - 1900) m d
