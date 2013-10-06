{-# LANGUAGE
  FlexibleContexts
, ScopedTypeVariables
  #-}

module Hadan.AlorTrade where


import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Internal (ResumableSource(..))
import Data.Conduit.Text
import Data.Time
import Hadan.Data.Candle
import Hadan.Data.Parsers.AlorTrade
import Network.HTTP.Conduit
import Network.URL
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Util as U
import qualified Data.Text as T
  

data Board = MICEX
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
                        

type Ticker = T.Text

-- | Download one bunch of data
downloadOnce :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                => Manager -> Board -> Ticker -> Period -> UTCTime -> m (ResumableSource m Candle)
downloadOnce manager board ticker period from = do
  req <- liftIO $ parseUrl url
  resp <- http req manager
  let (ResumableSource s fin) = responseBody resp
  return $ ResumableSource (s
                            $= decode utf8
                            $= (conduitParser $ parseCandle (T.pack $ show board) ticker $ periodToMinutes period)
                            $= L.map snd) fin
    
  
  where
    url = exportURL
          $ URL
          (Absolute
           $ Host (HTTP True) "history.alor.ru" Nothing)
          "" [("board", show board),
              ("ticker", T.unpack ticker),
              ("period", show $ periodToMinutes period),
              ("from", show from),
              ("bars", "1000")]


downloadCandles :: forall m. (MonadIO m, MonadResource m, MonadBaseControl IO m)
                   => Manager -> Board -> Ticker -> Period -> UTCTime -> Maybe UTCTime -> Source m Candle
downloadCandles manager board ticker period frm to = downloadCandles' frm
  where
    downloadCandles' from = do
      res <- downloadOnce manager board ticker period from
      -- lift $ res $$+- yieldUP
      -- ((), maxt) <- res $$+- filterTo
      --               =$ U.zipSinks yieldUP (L.fold (\x y -> max x (cTime y)) from)
      -- when (maxt > from)
      --   $ downloadCandles' $ plusMin maxt
      undefined

    -- yieldUP :: Sink Candle (Source m Candle) ()
    yieldUP = do
      x <- await
      case x of
        Nothing -> return ()
        Just cndl -> lift $ yield cndl

    filterTo = undefined
    plusMin = undefined
