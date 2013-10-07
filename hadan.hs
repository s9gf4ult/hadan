{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.HDBI
import Data.Conduit.Text
import Data.Time
import Database.HDBI
import Database.HDBI.SQlite
import Hadan.AlorTrade
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
      

main = bracket
       (connectSqlite3 "out.sqlite")
       disconnect $ \con -> withTransaction con $ do
         withManager $ \manager -> do
           feedCandles manager MICEX "GAZP" PMin Nothing
             $$ insertAllRows con "insert into candles(board, ticker, period, time, open, close, high, low, volume) values (?,?,?,?,?,?,?,?,?)"


       
