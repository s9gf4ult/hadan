
module Hadan.Data.Parsers.AlorTrade where

import Data.Attoparsec.Text
import Hadan.Data.Candle
import Data.Time
import qualified Data.Text as T

parseDay :: Parser Day
parseDay = do
  y <- decimal
  char '-'
  m <- decimal
  char '-'
  d <- decimal
  return $ fromGregorian y m d

parseDiffTime :: Parser DiffTime
parseDiffTime = do
  h <- decimal
  char ':'
  m <- decimal
  char ':'
  s <- decimal
  return $ secondsToDiffTime $ s + m*60 + h*3600

parseDate :: Parser UTCTime
parseDate = do
  day <- parseDay
  skipSpace
  t <- parseDiffTime
  return $ UTCTime day t

parseCandle :: T.Text -> T.Text -> Integer -> Parser Candle
parseCandle board ticker period = do
  skipSpace
  d <- parseDate
  skipSpace
  op <- rational
  skipSpace
  hi <- rational
  skipSpace
  lo <- rational
  skipSpace
  clos <- rational
  skipSpace
  vol <- rational
  skipSpace
  return $ Candle
    { cBoard = board
    , cTicker = ticker
    , cPeriod = period
    , cTime = d
    , cOpen = op
    , cClose = clos
    , cHigh = hi
    , cLow = lo
    , cVolume = vol }
    
