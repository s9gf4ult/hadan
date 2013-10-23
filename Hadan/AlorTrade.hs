{-# LANGUAGE
  FlexibleContexts
, ScopedTypeVariables
, BangPatterns
  #-}

module Hadan.AlorTrade where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Time
import Hadan.Data.Candle
import Hadan.Data.Parsers.AlorTrade
import Hadan.HTTP
import Network.HTTP.Conduit
import Network.URL
import System.Locale
import qualified Data.Text as T


data AlorBoard = MICEX
               | FORTS
               | FUTURES
               deriving (Show, Eq)

data Period = PMin
            | P5Min
            | P10Min
            | P15Min
            | P20Min
            | P30Min
            | PHour
            | PDay

periodToMinutes :: (Num a) => Period -> a
periodToMinutes PMin = 1
periodToMinutes P5Min = 5
periodToMinutes P10Min = 10
periodToMinutes P15Min = 15
periodToMinutes P20Min = 20
periodToMinutes P30Min = 30
periodToMinutes PHour = 60
periodToMinutes PDay = 1440


-- | Download one bunch of data
downloadOnce :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                => Manager -> AlorBoard -> Ticker -> Period -> Maybe UTCTime -> Source m Candle
downloadOnce manager board ticker period to = streamHttp manager url []
                                              $ parseCandle (Board $ T.pack $ show board) ticker
                                              $ periodToMinutes period
  where
    url = exportURL
          $ URL
          (Absolute
           $ Host (HTTP False) "history.alor.ru" Nothing)
          "" $ [("board", show board),
                ("ticker", T.unpack $ unTicker ticker),
                ("period", show (periodToMinutes period :: Int)),
                ("bars", "1000")] ++ case to of
            Nothing -> []
            Just fto -> [("to", formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fto)]


-- | If upper time limit is empty get current time as start upper time
downloadCandles :: forall m. (MonadIO m, MonadResource m, MonadBaseControl IO m)
                   => Manager -> AlorBoard -> Ticker -> Period -> Maybe UTCTime -> Source m Candle
downloadCandles manager board ticker period gto = downloadCandles' gto
  where
    downloadCandles' :: Maybe UTCTime -> Source m Candle
    downloadCandles' to = do
      foldtime <- case to of
        Nothing -> liftIO getCurrentTime
        Just nto -> return nto
      mint <- downloadOnce manager board ticker period to
              =$= (yieldFold foldtime)
      when (mint < foldtime)
        $ downloadCandles' $ Just $ subMin mint

    yieldFold !mint = do
      n <- await
      case n of
        Nothing -> return mint
        Just cndl -> do
          yield cndl
          yieldFold $ min mint $ cTime cndl
    subMin = addUTCTime (-60)
