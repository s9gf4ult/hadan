{-# LANGUAGE
  FlexibleContexts
  #-}

module Hadan.HTTP where

import Codec.Text.IConv
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Attoparsec.Text
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Internal (ResumableSource(..))
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



-- | Stream
streamHttp :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
              => Manager
              -> String          -- ^ url
              -> RequestHeaders  -- ^ request headers
              -> Parser a        -- ^ parser to get input
              -> m (ResumableSource m a)
streamHttp manager url hdrs parser = do
  liftIO $ hPutStrLn stderr $ "streaming: " ++ url
  lreq <- liftIO $ parseUrl url
  let req = lreq {requestHeaders = hdrs}
  resp <- http req manager
  let (ResumableSource s fin) = responseBody resp
  return $ ResumableSource (s
                            $= decode utf8
                            $= conduitParserEither parser
                            $= noErrors) fin


  where
    noErrors = do
      n <- await
      case n of
        Nothing -> return ()
        Just next -> case next of
          Left _ -> noErrors
          Right (_, cndl) -> do
            yield cndl
            noErrors

streamManyHttp :: (MonadIO m, MonadResource m, MonadBaseControl IO m)
                  => Manager
                  -> Double                     -- ^ seconds to wait between queries
                  -> [(String, RequestHeaders)] -- ^ url & request headers
                  -> Parser a
                  -> Source m a
streamManyHttp _ _ [] _ = return ()
streamManyHttp man del ((url, hdrs):urls) parser = do
  (source, _) <- lift $ do
    src <- streamHttp man url hdrs parser
    unwrapResumable src
  source
  when (delay > 0) $ liftIO $ do
    hPutStrLn stderr $ "delaying for " ++ show delay ++ " seconds"
    threadDelay $ round $ delay * 1e6
  streamManyHttp man delay urls parser
  where
    delay = max 0 del
