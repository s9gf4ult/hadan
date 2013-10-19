
module Hadan.Data.Parsers.AlorTrade where

import Data.Attoparsec.Text
import Hadan.Data.Candle
import Data.Time

parseDay :: Parser Day
parseDay = do
  y <- decimal
  _ <- char '-'
  m <- decimal
  _ <- char '-'
  d <- decimal
  return $ fromGregorian y m d

parseDiffTime :: Parser DiffTime
parseDiffTime = do
  h <- decimal
  _ <- char ':'
  m <- decimal
  _ <- char ':'
  s <- decimal
  return $ secondsToDiffTime $ s + m*60 + h*3600

parseDate :: Parser UTCTime
parseDate = do
  day <- parseDay
  skipSpace
  t <- parseDiffTime
  return $ UTCTime day t

parseCandle :: Board -> Ticker -> Integer -> Parser Candle
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
    
