{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Attoparsec
import Data.Conduit.Text
import Hadan.Data.Candle
import Hadan.Data.Parsers.AlorTrade
import Network.HTTP.Conduit
import qualified Data.Conduit.List as L

sinkCandle :: (MonadIO m) =>  Sink (PositionRange, Candle) m ()
sinkCandle = do
  c <- await
  case c of
    Nothing -> return ()
    Just (_, cndl) -> do
      liftIO $ print cndl
      sinkCandle
      

main = do
  req <- parseUrl "http://history.alor.ru/?board=MICEX&ticker=GAZP&period=1&from=2013-09-01+9%3A0%3A0&to=&bars=100"
  withManager $ \mngr -> do
    resp <- http req mngr
    responseBody resp
      $$+- decode utf8
      =$ (conduitParser $ parseCandle "MICEX" "GAZP" 1)
      =$ sinkCandle
            
            
