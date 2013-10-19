{-# LANGUAGE
  QuasiQuotes
, FlexibleContexts
, OverloadedStrings
  #-}
module Hadan.Finam where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.Time
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
  where
    fhost = Host (HTTP False) "www.finam.ru" Nothing

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
