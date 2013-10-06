{-# LANGUAGE
  FlexibleContexts
, ScopedTypeVariables
  #-}

module Hadan.AlorTrade where

import System.IO (stderr, hPutStrLn)
import System.Locale
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

-- import Debug.Trace
  
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
                => Manager -> Board -> Ticker -> Period -> Maybe UTCTime -> m (ResumableSource m Candle)
downloadOnce manager board ticker period to = do
  liftIO $ hPutStrLn stderr url
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
           $ Host (HTTP False) "history.alor.ru" Nothing)
          "" $ [("board", show board),
                ("ticker", T.unpack ticker),
                ("period", show $ periodToMinutes period),
                ("bars", "1000")] ++ case to of
            Nothing -> []
            Just fto -> [("to", formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fto)]


feedCandles :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
               => Manager -> Board -> Ticker -> Period -> Maybe UTCTime -> Sink Candle m ignore -> m ()
feedCandles manager board ticker period gto sink = feedCandles' gto
  where
    feedCandles' to = do
      foldtime <- case to of
        Nothing -> liftIO getCurrentTime
        Just tt -> return tt
      res <- downloadOnce manager board ticker period to
      (_, mint) <- res $$+- U.zipSinks sink (L.fold (\t c -> min t $ cTime c) foldtime)
      when (mint < foldtime)
        $ feedCandles' $ Just $ subMin mint
    subMin = addUTCTime (-60)


-- downloadCandles :: forall m. (MonadIO m, MonadResource m, MonadBaseControl IO m)
--                    => Manager -> Board -> Ticker -> Period -> UTCTime -> Maybe UTCTime -> Source m Candle
-- downloadCandles manager board ticker period frm to = downloadCandles' frm
--   where
--     downloadCandles' :: UTCTime -> Source m Candle
--     downloadCandles' from = do
--       (ResumableSource res fin) <- lift $ downloadOnce manager board ticker period from
--       let lres = ResumableSource (transPipe lift res) fin
--       -- lres $$+- yieldUP

--       -- lift $ res $$+- yieldUP
--       -- ((), maxt) <- res $$+- filterTo
--       --               =$ U.zipSinks yieldUP (L.fold (\x y -> max x (cTime y)) from)
--       -- when (maxt > from)
--       --   $ downloadCandles' $ plusMin maxt
--       undefined

--     yieldUP :: Sink Candle (ConduitM () Candle m) ()
--     yieldUP = do
--       x <- await
--       case x of
--         Nothing -> return ()
--         Just cndl -> lift $ yield cndl

--     filterTo = undefined
--     plusMin = undefined
