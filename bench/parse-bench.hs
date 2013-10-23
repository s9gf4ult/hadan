{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
  #-}

module Main where

-- import Database.HDBI.PostgreSQL
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Exception
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary (sourceFile, sinkFile)
import Data.Conduit.HDBI
import Data.Conduit.Text(decode, utf8)
import Database.HDBI
import Hadan.Data.Candle
import Hadan.Data.Parsers.Finam
import System.Environment
import qualified Data.Conduit.List as L

  
main = do -- bracket (connectSqlite3 "user=test dbname=test") disconnect $ \con -> do
  -- runRaw con "CREATE TABLE ticks(id integer primary key, board, ticker, time, price, volume)"
  args <- getArgs
  let fname = head args
  runResourceT $ do
    a <- sourceFile fname
         $= decode utf8
         $= conduitParserEither (parseTick $ Board "MICEX")
         $= justRights
         $= asThisType (undefined :: Tick)
         $= flushAt 1000
         $= unflush
         $= L.mapM (liftIO . evaluate)
         $$ L.fold (\(a::Int) _ -> a + 1) 0
  -- Just (a :: Int) <- runFetchOne con "select count(*) from ticks" ()
    liftIO $ print a


justRights = do
  n <- await
  case n of
    Nothing -> return ()
    Just nx -> case nx of
      Right (_, x) -> do
        yield x
        justRights
      Left _ -> justRights

unflush = do
  n <- await
  case n of
    Nothing -> return ()
    Just (Chunk a) -> do
      yield a
      unflush
    _ -> unflush
