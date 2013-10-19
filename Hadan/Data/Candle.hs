{-# LANGUAGE
  TemplateHaskell
, GeneralizedNewtypeDeriving
, DeriveDataTypeable
  #-}

module Hadan.Data.Candle where


import Data.Data
import Data.Time
import Data.Decimal
import Database.HDBI
import Data.Monoid
import Data.String
import Language.Haskell.TH.HDBI
import qualified Data.Text as T

newtype Ticker = Ticker {unTicker :: T.Text}
                 deriving (Show, Read, Eq, FromSql, ToSql, Ord, Monoid, IsString, Typeable)
                 
newtype Board = Board {unBoard :: T.Text}
                deriving (Show, Read, Eq, FromSql, ToSql, Ord, Monoid, IsString, Typeable)
data Candle = Candle
              { cBoard :: Board
              , cTicker :: Ticker
              , cPeriod :: Integer
              , cTime :: UTCTime
              , cOpen :: Decimal
              , cClose :: Decimal
              , cHigh :: Decimal
              , cLow :: Decimal
              , cVolume :: Decimal
              } deriving (Show, Eq, Typeable)

$(deriveFromRow ''Candle)
$(deriveToRow ''Candle)


data Tick = Tick
            { tBoard :: Board
            , tTicker :: Ticker
            , tTime :: UTCTime
            , tPrice :: Decimal
            , tVolume :: Decimal
            } deriving (Show, Eq, Typeable)

$(deriveFromRow ''Tick)
$(deriveToRow ''Tick)
