module Hadan.URL where

import Network.URL


mutUrl :: (URL -> URL) -> String -> String
mutUrl mut s = case importURL s of
  Nothing -> s
  Just u -> exportURL $ mut u

-- | replace relative host to concrete
fixUrlHost :: Host -> URL -> URL
fixUrlHost lhost url@(URL {url_type = ut}) = case ut of
  HostRelative -> url { url_type = Absolute lhost}
  _ -> url


appendParameters :: [(String, String)] -> URL -> URL
appendParameters params url@(URL {url_params = oldp}) = url {url_params = oldp ++ params}
