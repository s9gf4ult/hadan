{-# LANGUAGE
  QuasiQuotes
, FlexibleContexts
, OverloadedStrings
  #-}
module Hadan.Finam where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Time
import Hadan.Data.Candle
import Hadan.Data.Parsers.Finam
import Hadan.HTTP
import Hadan.URL
import Hadan.XML
import Network.HTTP.Conduit
import Network.URL
import Text.Printf
import Text.XML.Cursor
import qualified Data.Text as T

-- import Debug.Trace


-- | .sect-menu > table:nth-child(1) > tr:nth-child(1) > td:nth-child(5) > a:nth-child(1)
graphicsAxis :: Axis
graphicsAxis = descendant >=> attributeIs "class" "sect-menu"
           >=> child >=> element "table"
           >=> child >=> element "tr"
           >=> child >=> element "td"
           >=> child >=> element "a" >=> findByContent (== "Графики")


-- | tr.last:nth-child(13) > td:nth-child(1) > a:nth-child(1)
exportsAxis :: Axis
exportsAxis = descendant >=> element "tr" >=> attributeIs "class" "last"
                >=> child >=> element "td"
                >=> child >=> element "a" >=> findByContent (== "Экспорт котировок")

-- | form#chartform
chartformAxis :: Axis
chartformAxis = descendant >=> element "form" >=> attributeIs "id" "chartform"


downloadFollower :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                    => Manager
                    -> m (T.Text, T.Text) -- ^ Follower link and form action
downloadFollower man = do
  gr <- mutUrl (fixUrlHost fhost)
        . T.unpack
        . getAttrByAxis "href" graphicsAxis
        <$> downloadDoc man "http://www.finam.ru"
  follower <- mutUrl (fixUrlHost fhost)
              . T.unpack
              . getAttrByAxis "href" exportsAxis
              <$> downloadDoc man gr
  action <- getAttrByAxis "action" chartformAxis
            <$> downloadDoc man follower
  return (T.pack follower, action)


-- | host finam.ru
fhost :: Host
fhost = Host (HTTP False) "www.finam.ru" Nothing

downloadTicks :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                 => Manager
                 -> Double
                 -> String       -- ^ Ticker name
                 -> Day          -- ^ from date
                 -> Day          -- ^ to date
                 -> Source m Tick
downloadTicks manager delay stock from to = do
  (fol, act) <- lift $ downloadFollower manager
  streamManyHttp manager delay (zip (urls act) $ repeat $ hdr fol) $ parseTick $ Board "MICEX"
  where
    urls act = map exportURL $ dayUrls lurl stock
               $ takeWhile (>= from) $ daysBack to
      where
        lurl = case importURL $ T.unpack act of
          Nothing -> error $ "could not parse action url: " ++ show act
          Just u -> u
    hdr fol = [("Referer", toByteString $ fromText fol)]


-- | Infinite list of days going back in time
daysBack :: Day -> [Day]
daysBack day = iterate (addDays (-1)) day

dayUrls :: URL -> String -> [Day] -> [URL]
dayUrls url stock days = map (\day -> tickUrl day day stock url) days

tickUrl :: Day -> Day -> String -> URL -> URL
tickUrl from to stock url = url {url_path = (fname ++ ".txt"),
                                 url_params = tickParams}
  where
    tickParams :: [(String, String)]
    tickParams = [("market", "1")
                 ,("em", "3")
                 ,("code", stock)
                 ,("df", show df)
                 ,("mf", show $ mf - 1) -- fucken magic
                 ,("yf", show yf)
                 ,("dt", show dt)
                 ,("mt", show $ mt - 1)
                 ,("yt", show yt)
                 ,("p", "1")
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
                 ,("datf", "6")]
    (yf, mf, df) = toGregorian from
    (yt, mt, dt) = toGregorian to
    fname = printf "%s_%s_%s" stock (fmtdt yf mf df) (fmtdt yt mt dt)
    fmtdt :: Integer -> Int -> Int -> String
    fmtdt y m d = printf "%02d%02d%02d" (if y >= 2000 then y - 2000 else y - 1900) m d
