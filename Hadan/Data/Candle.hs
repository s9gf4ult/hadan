{-# LANGUAGE
  TemplateHaskell
  #-}

module Hadan.Data.Candle where


import Data.Time
import Data.Decimal
import Language.Haskell.TH.HDBI
import qualified Data.Text as T

data Candle = Candle
              { cBoard :: T.Text
              , cTicker :: T.Text
              , cPeriod :: Integer
              , cTime :: UTCTime
              , cOpen :: Decimal
              , cClose :: Decimal
              , cHigh :: Decimal
              , cLow :: Decimal
              , cVolume :: Decimal
              } deriving (Show, Eq)

$(deriveFromRow ''Candle)
$(deriveToRow ''Candle)
