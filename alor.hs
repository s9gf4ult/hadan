{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.HDBI
import Data.Function (on)
import Data.List (intercalate)     
import Data.Monoid      
import Data.Time
import Database.HDBI
import Database.HDBI.SQlite
import Hadan.AlorTrade
import Hadan.Data.Candle
import Hadan.URL
import Network.HTTP.Conduit
import Network.URL
import System.Environment(getArgs)
import qualified Data.Conduit.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL      

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
     bracket (connectSqlite3 "out1.sqlite") disconnect $ \con -> do
       args <- getArgs
       let ticker = args !! 0
           -- year = read $ args !! 1
           -- month = read $ args !! 2
           -- day = read $ args !! 3
       withManagerSettings (def {managerResponseTimeout = Just 180000000}) $ \man -> do
         downloadCandles man MICEX (Ticker $ T.pack ticker) PMin Nothing
           $= flushBy ((==) `on` utctDay . cTime)
           $$ (insertAllTrans con
               $ "insert into candles (board, ticker, period, time, open, close, high, low, volume) values ("
                                      <> (Query $ TL.pack $ intercalate "," $ replicate 9 "?") <> ")")

       
