{-# LANGUAGE
  OverloadedStrings
, TemplateHaskell
  #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Text
import Data.Time
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
      

main = do
  withManager $ \manager -> do
    feedCandles manager MICEX "GAZP" PMin
      Nothing
      (L.mapM_ $ lift . print)
