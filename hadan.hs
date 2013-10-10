{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Data.Time
import Hadan.Finam
import Network.HTTP.Conduit
import Network.URL
import qualified Data.Text as T


import Debug.Trace
-- sinkCandle :: (MonadIO m) =>  Sink (PositionRange, Candle) m ()
-- sinkCandle = do
--   c <- await
--   case c of
--     Nothing -> return ()
--     Just (_, cndl) -> do
--       liftIO $ print cndl
--       sinkCandle
strs x = trace (show x) x

main = do
  withManagerSettings (def {managerResponseTimeout = Just 90000000}) $ \man -> do
    (fol, ac) <- downloadFollower man
    lreq <- liftIO $ parseUrl $ strs
            $ (mutUrl $ tickUrl (fromGregorian 2013 10 1) (fromGregorian 2013 10 3) "SBER")
            $ T.unpack ac
    let req = lreq {requestHeaders = [("Referer", toByteString $ fromText fol)]}
    liftIO $ print req
    resp <- http req man
    responseBody resp $$+- sinkFile "out.txt"

       
