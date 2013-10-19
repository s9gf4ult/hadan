{-# LANGUAGE
  FlexibleContexts
  #-}

module Hadan.HTTP where


import Codec.Text.IConv
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Conduit
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
