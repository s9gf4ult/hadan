{-# LANGUAGE
  ScopedTypeVariables
  #-}

module Hadan.Data.Parsers.Finam where

import Prelude hiding (take, takeWhile)

import Data.Attoparsec.Text
import Data.Char
import Data.Time
import Hadan.Data.Candle
import qualified Data.Text as T

readPar :: (Monad m, Integral a) => T.Text -> m a
readPar t = case parseOnly decimal t of
  Right res -> return res
  Left e -> fail $ "could not read decimal: " ++ e

finamDate :: Parser Day
finamDate = fd <?> "Finam date"
  where
    fd = do
      y <- readPar =<< take 4
      m <- readPar =<< take 2
      d <- readPar =<< take 2
      return $ fromGregorian y m d

finamTime :: Parser DiffTime
finamTime = ft <?> "Finam time"
  where
    ft = do
      h <- readPar =<< take 2
      m <- readPar =<< take 2
      s <- readPar =<< take 2
      return $ secondsToDiffTime $ s + (m * 60) + (h * 3600)

parseTick :: Board -> Parser Tick
parseTick board = do
  skipSpace <?> "first skip"
  tckr <- takeWhile isAlpha
  coma
  (_ :: Int) <- decimal
  coma
  date <- finamDate
  coma
  time <- finamTime
  coma
  price <- rational <?> "price"
  coma
  vol <- rational <?> "volume"
  skipSpace <?> "last skip"
  return
    $ Tick { tBoard = board
           , tTicker = Ticker tckr
           , tTime = (UTCTime date time)
           , tPrice = price
           , tVolume = vol
           }
  where
    coma = (char ',' >> return ()) <?> "coma"
