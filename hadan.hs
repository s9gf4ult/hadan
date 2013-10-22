{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import System.Environment(getArgs)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.HDBI
import Data.Function (on)
import Data.Time
import Database.HDBI
import Database.HDBI.SQlite
import Hadan.Data.Candle
import Hadan.Finam
import Hadan.URL
import Network.HTTP.Conduit
import Network.URL
import qualified Data.Conduit.List as L
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

main = bracket (connectSqlite3 "out.sqlite") disconnect $ \con -> do
  args <- getArgs
  let ticker = args !! 0
      year = read $ args !! 1
      month = read $ args !! 2
      day = read $ args !! 3
  withManagerSettings (def {managerResponseTimeout = Just 180000000}) $ \man -> do
    downloadTicks man 3 ticker (fromGregorian 2000 1 1) (fromGregorian year month day)
      $= flushBy ((==) `on` utctDay . tTime)
      $$ insertAllTrans con "insert into ticks (board, ticker, time, price, volume) values (?,?,?,?,?)"

       
