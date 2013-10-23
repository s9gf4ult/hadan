{-# LANGUAGE
  FlexibleContexts
, ScopedTypeVariables
  #-}

module Hadan.HTTP where

import Codec.Text.IConv
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Attoparsec.Text (Parser)
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Text
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.IO (stderr, hPutStrLn)
import Text.HTML.DOM
import Text.XML (Document(..))


downloadDoc :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
               => Manager
               -> String         -- ^ URL to download from
               -> m Document
downloadDoc manager url = do
  req <- liftIO $ parseUrl url
  resp <- http req manager
  lbs <- responseBody resp $$+- sinkLbs
  let convlbs = convert "CP1251" "UTF-8" lbs
  return $ parseLBS convlbs



-- | Try to download data and ignore any excpetions until download is
-- complete. Then steram parsed data ignoring parser errors
streamHttp :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
              => Manager
              -> String          -- ^ url
              -> RequestHeaders  -- ^ request headers
              -> Parser a        -- ^ parser to get input
              -> Source m a
streamHttp manager url hdrs parser = do
  lreq <- liftIO $ do
    hPutStrLn stderr $ "downloading: " ++ url
    parseUrl url
  let req = lreq {requestHeaders = hdrs}
  resp <- lift $ download req
  let lbs = responseBody resp
  prnt "streaming..."
  sourceLbs lbs
    $= decode utf8
    $= conduitParserEither parser
    $= noErrors
  where
    download req = do
      r <- try $ do
        httpLbs req manager
      case r of
        Left (e :: SomeException) -> do
          prnt $ "Exception occured: " ++ show e
          download req
        Right resp -> do
          prnt "download complete"
          return resp

    noErrors = do
      n <- await
      case n of
        Nothing -> return ()
        Just next -> case next of
          Left _ -> noErrors
          Right (_, cndl) -> do
            yield cndl
            noErrors
    prnt x = liftIO $ hPutStrLn stderr x





streamManyHttp :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                  => Manager
                  -> Double                     -- ^ seconds to wait between queries
                  -> [(String, RequestHeaders)] -- ^ url & request headers
                  -> Parser a
                  -> Source m a
streamManyHttp _ _ [] _ = return ()
streamManyHttp man del ((url, hdrs):urls) parser = do
  streamHttp man url hdrs parser
  when (delay > 0) $ liftIO $ do
    hPutStrLn stderr $ "delaying for " ++ show mdel ++ " seconds"
    threadDelay delay
  streamManyHttp man mdel urls parser
  where
    mdel = max 0 del
    delay = round $ mdel * 1e6
