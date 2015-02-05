module Data.Json.Render 
  ( renderJTableRaw
  ) where

import Data.Either
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple
import Data.String (joinWith)
import Data.Array
import qualified Data.Array.Unsafe as AU
import qualified Data.StrMap as M
import Data.Foldable (foldl, any, all, mconcat)
import Math (max)

import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Data.Argonaut.Parser (jsonParser)
import Text.Smolder.HTML (table, thead, tbody, tr, th, td)
import Text.Smolder.Markup (Markup(..), MarkupM(..), Attribute(..), attribute, text, (!))


data Tree = T String Number Number [Tree]
tLabel (T l _ _ _) = l
tWidth (T _ w _ _) = w
tHeight (T _ _ h _) = h
tChildren (T _ _ _ c) = c

instance showTree :: Show Tree where
  show (T l w h c) = joinWith " " ["<T", l, show w, show h, show c, ">"]

data Cell = C Number Number Json
cWidth (C w _ _) = w
cHeight (C _ h _) = h
cJson (C _ _ j) = j

foreign import jnull "var jnull = null;" :: Json


isTuple :: [Json] -> Maybe Number
isTuple ja = if length ja <= 1 then Nothing else
  let f = foldJson (const 0) (const 1) (const 2) (const 3) (const 4) (const 4)
      types = ja <#> f
      has_arr_or_obj = any ((==) 4) types
      all_eq = all ((==) $ AU.head types) types
  in if has_arr_or_obj || (all_eq && length ja > 2) then Nothing 
                                                    else Just $ length ja


tMerge :: Tree -> Tree -> Tree
tMerge (T l w h c) (T l1 w1 h1 c1) =
  let i = findIndex (\n -> l1 == (n # tLabel)) c in case c !! i of
    Just (T l2 w2 h2 c2) ->
      let c2' = tChildren $ foldl tMerge (T l2 w2 h2 c2) c1
          c' = updateAt i (T l2 w2 h2 c2') c
          w' = w - w2 + (max w1 w2)
          h' = max h (h2 + 1)
      in  T l w' h' c'
    Nothing -> T l (w+w1) 1 (snoc c (T l1 w1 h1 c1))


tFromJson :: String -> Json -> Tree
tFromJson label json =
  case json # toObject of -- object
    Just jo -> let c = map (\(Tuple l j) -> tFromJson l j) (M.toList jo)
                   w = foldl (+) 0 (c <#> tWidth)
                   h = 1 + foldl max 0 (c <#> tHeight)
               in T label w h c
    Nothing -> case json # toArray of
      Just ja -> case isTuple ja of
        Just n -> T label n 0 [] -- tuple
        Nothing -> -- array
          let t = ja <#> tFromJson "" 
              w = foldl max 0 (t <#> tWidth)
              ts = t >>= tChildren
              h = 1 + foldl max 0 (ts <#> tHeight)
              t' = foldl tMerge (T label 0 0 []) ts
              h' = if null (t' # tChildren) then 0 else h
          in T label (max w (t' # tWidth)) h' (t' # tChildren)
      Nothing -> T label 1 0 [] -- primitive


_nattr :: String -> Number -> Markup -> Markup
_nattr attr n m = if n > 1 then m ! (attribute attr $ show n) else m
_cspan = _nattr "colspan"
_rspan = _nattr "rowspan"


renderThead :: Tree -> Markup
renderThead root = mconcat $ do 
  let h = tHeight root
  let toRows ts = if null ts then [] else ts : toRows (ts >>= tChildren)
  let rows = toRows (root # tChildren)
  (Tuple row i) <- rows `zip` (0 .. h)
  return $ tr $ mconcat $ do
    t <- row
    let rs = if null (t # tChildren) then h - i else 1
    return $ th >>> _cspan (t # tWidth) >>> _rspan rs $ text (t # tLabel)


mergeO :: [[[Cell]]] -> [[Cell]]
mergeO rss = do
  let maxh = foldl (\n l -> max n $ length l) 0 rss
  let fol = \n cs -> max n $ foldl (+) 0 (cs <#> cWidth)
  let maxws = rss <#> \rs -> foldl fol 0 rs
  n <- 0 .. maxh-1
  return $ concat $ do
    (Tuple rs w)<- rss `zip` maxws
    return $  if length rs == 1 
              then if n == 0 then (\(C w h j) -> C w maxh j) `map` (AU.head rs)
                             else fromMaybe [] $ rs !! n 
              else fromMaybe [C w 1 jnull] $ rs !! n


cFromJson :: Tree -> Json -> [[Cell]]
cFromJson t json = 
  case json # toObject of 
    Just jo -> mergeO $ do
      t' <- t # tChildren
      let label = t' # tLabel
      let j = fromMaybe jnull $ M.lookup label jo
      return $ cFromJson t' j
    Nothing -> case json # toArray of
      Just ja -> 
        if (t # tHeight) > 0 || (t # tWidth) <= 1 || not (isJust $ isTuple ja)
        then ja >>= cFromJson t
        else singleton $ do
          n <- 0 .. (t # tWidth) - 1
          let j = fromMaybe jnull $ ja !! n
          return $ C 1 1 j
      Nothing -> [[C (t # tWidth) 1 json]]


renderTbody :: Tree -> Json -> Markup
renderTbody t json = mconcat $ do
  row <- cFromJson t json
  return $ tr $ mconcat $ do
    cell <- row
    return $ td >>> _cspan (cell #cWidth) 
                >>> _rspan (cell # cHeight) 
                $ text $ show (cell # cJson)


renderJTableRaw :: Json -> Markup
renderJTableRaw json = table $ do 
  let t = tFromJson "" json
  thead $ renderThead t
  tbody $ renderTbody t json

