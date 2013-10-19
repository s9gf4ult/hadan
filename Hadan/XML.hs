
module Hadan.XML where

import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import qualified Data.Map as M  

-- | matches elements who is nth-child starting from 1. Just like CSS
-- pseudoclass nth-child
nthChild :: Int -> Axis
nthChild n = check isnth
  where
    isnth cur = (n-1) == (length $ precedingSibling cur)


-- | Find all elements who's content matches prdicate
findByContent :: (T.Text -> Bool) -> Axis
findByContent prd = check getCont
  where
    getCont cur = any prd $ (child >=> content) cur
    
-- | Find exactly one element with axis and get attribute from it
getAttrByAxis :: Name -> Axis -> Document -> T.Text
getAttrByAxis attr axis doc = case axis $ fromDocument doc of
  [cur] -> getAttr cur attr
  x     -> error $ "found " ++ (show $ length x) ++ " elements, but must be 1"

getAttr :: Cursor -> Name -> T.Text
getAttr cur name = case node cur of
  (NodeElement (Element _ attrs _)) ->
    case M.lookup name attrs of
      Nothing -> error $ show name ++ " attribute not found"
      Just r -> r
  _ -> error "Not a NodeElement"
  
