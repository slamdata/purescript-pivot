module Data.Json.JTable.Internal where

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
import Text.Smolder.HTML (tr, th, td)
import Text.Smolder.Markup (Markup(..), Attribute(..), attribute, (!))


data Tree = T [String] Number Number [Tree]
tPath (T p _ _ _) = p
tWidth (T _ w _ _) = w
tHeight (T _ _ h _) = h
tKids (T _ _ _ k) = k

instance showTree :: Show Tree where
  show (T p w h k) = joinWith " " ["<T", show p, show w, show h, show k, ">"]

data Cell = C Number Number JCursor Json
cWidth (C w _ _ _) = w
cHeight (C _ h _ _) = h
cCursor (C _ _ c _) = c
cJson (C _ _ _ j) = j

instance showCell :: Show Cell where
  show (C w h c j) = joinWith " " ["<T", show w, show h, show c, show j, ">"]

foreign import jnull "var jnull = null;" :: Json

type MarkupF = Markup -> Markup


isTuple :: [Json] -> Maybe Number
isTuple ja = if length ja <= 1 then Nothing else
  let f = foldJson (const 0) (const 1) (const 2) (const 3) (const 4) (const 4)
      types = ja <#> f
      has_arr_or_obj = any ((==) 4) types
      all_eq = all ((==) $ AU.head types) types
  in if has_arr_or_obj || (all_eq && length ja > 2) then Nothing 
                                                    else Just $ length ja


tMergeArray :: Tree -> Tree -> Tree
tMergeArray (T p w h k) (T p1 w1 h1 k1) =
  let i = findIndex (\n -> last p1 == last (n # tPath)) k in case k !! i of
    Just t2@(T p2 w2 h2 k2) ->
      let k2' = tKids $ foldl tMergeArray t2 k1
          k' = updateAt i (T p2 (max w1 w2) h2 k2') k
          w' = w - w2 + (max w1 w2)
          h' = max h (h2 + 1)
      in  T p w' h' k'
    Nothing -> T p (w+w1) 1 (snoc k (T p1 w1 h1 k1))


tFromJson :: [String] -> Json -> Tree
tFromJson path json =
  case json # toObject of -- object
    Just jo -> let k = map (\(Tuple l j) -> tFromJson (snoc path l) j) (M.toList jo)
                   w = foldl (+) 0 (k <#> tWidth)
                   h = 1 + foldl max 0 (k <#> tHeight)
               in T path w h k
    Nothing -> case json # toArray of
      Just ja -> case isTuple ja of
        Just n -> T path n 0 [] -- tuple
        Nothing -> -- array
          let ts = ja <#> tFromJson path
              tk = ts >>= tKids
              h = 1 + foldl max 0 (tk <#> tHeight)
              t' = foldl tMergeArray (T path 0 0 []) tk
              w = foldl max (t' # tWidth) (ts <#> tWidth)
              h' = if null (t' # tKids) then 0 else h
          in T path w h' (t' # tKids)
      Nothing -> T path 1 0 []


_nattr :: String -> Number -> Markup -> Markup
_nattr attr n m = if n > 1 then m ! (attribute attr $ show n) else m
_cspan = _nattr "colspan"
_rspan = _nattr "rowspan"


renderThead :: MarkupF -> ([String] -> Markup) -> Tree -> Markup
renderThead tr' thf root = mconcat $ do 
  let height = tHeight root
  let toRows ts = if null ts then [] else ts : toRows (ts >>= tKids)
  let rows = toRows (root # tKids)
  (Tuple row i) <- rows `zip` (0 .. height)
  return $ tr' $ mconcat $ do
    (T p w h k) <- row
    let rs = if null k then height - i else 1
    return $ (thf p # _cspan w >>> _rspan rs)


cMergeObject :: [[[Cell]]] -> [[Cell]]
cMergeObject rss = do
  let maxh = foldl (\n l -> max n $ length l) 0 rss
  let fol = \n cs -> max n $ foldl (+) 0 (cs <#> cWidth)
  let maxws = rss <#> \rs -> foldl fol 0 rs
  n <- 0 .. maxh-1
  return $ concat $ do
    (Tuple rs w)<- rss `zip` maxws
    return $  if length rs == 1 
              then if n == 0 then (\(C w h c j) -> C w maxh c j) `map` (AU.head rs)
                             else fromMaybe [] $ rs !! n 
              else fromMaybe [C w 1 (JCursorTop) jnull] $ rs !! n


cFromJson :: Tree -> JCursor -> Json -> [[Cell]]
cFromJson t c json = 
  case json # toObject of 
    Just jo -> cMergeObject $ do -- object
      t' <- t # tKids
      let label = AU.last (t' # tPath)
      let j = fromMaybe jnull $ M.lookup label jo
      return $ cFromJson t' (downField label c) j
    Nothing -> case json # toArray of
      Just ja -> 
        if (t # tHeight) > 0 || (t # tWidth) <= 1 || not (isJust $ isTuple ja)
        then concat $ do -- array
          (Tuple j i)<- ja `zip` (0 .. length ja)
          return $ cFromJson t (downIndex i c) j
        else singleton $ do -- tuple
          i <- 0 .. (t # tWidth) - 1
          let j = fromMaybe jnull $ ja !! i
          return $ C 1 1 (downIndex i c) j
      Nothing -> [[C (t # tWidth) 1 c json]] -- primitive


renderTbody :: MarkupF -> (JCursor -> Json -> Markup) -> Tree -> Json -> Markup
renderTbody tr' tdf t json = mconcat $ do
  row <- cFromJson t JCursorTop json
  return $ tr' $ mconcat $ do
    (C w h c j) <- row
    return $ (tdf c j # _cspan w >>> _rspan h)


sortTree :: ([String] -> [String] -> Ordering) -> Tree -> Tree
sortTree ord (T p w h k) = T p w h ( 
  sortBy (\t1 t2 -> ord (t1 # tPath) (t2 # tPath)) (k <#> sortTree ord))


foreign import localeCompare """var localeCompare =
function (s1) { return function (s2) {
  return s1.localeCompare(s2) } }""" :: String -> String -> Number

strcmp :: String -> String -> Ordering
strcmp s1 s2 = compare (localeCompare s1 s2) 0



module Data.Json.JTable where

import Data.Json.JTable.Internal

import qualified Data.Array.Unsafe as AU
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Text.Smolder.HTML (table, thead, tbody, tr, th, td)
import Text.Smolder.Markup (Markup(..), MarkupM(..), Attributable, attribute, (!), text)


type TableStyle = {
  table :: Markup -> Markup,
  tr    :: Markup -> Markup ,
  th    :: [String] -> Markup,
  td    :: JCursor -> Json -> Markup }

renderJsonSimple = foldJson (const "&nbsp;") show show id (const "") (const "")

noStyle = {
  table: table, 
  tr: tr, 
  th: \p -> th $ text $ AU.last p,
  td: \c j -> td $ text $ renderJsonSimple j
    
} :: TableStyle

bootstrapStyle = (noStyle {
  table = \m -> table ! attribute "class" "table" $ m}) :: TableStyle

renderJsonSemantic :: JCursor -> Json -> String
renderJsonSemantic = \c j -> show j

semanticStyle = (noStyle {
  td = \c j -> td $ text $ renderJsonSemantic c j} :: TableStyle)


type ColumnOrdering = [String] -> [String] -> Ordering

inOrdering = (\p1 p2 -> EQ) :: ColumnOrdering
alphaOrdering = (\p1 p2 -> strcmp (AU.last p1) (AU.last p2)) :: ColumnOrdering


type JTableOpts = {
  style :: TableStyle,
  columnOrdering :: ColumnOrdering,
  insertHeaderCells :: Boolean }

defJTableOpts  = {
  style: noStyle,
  columnOrdering: inOrdering,
  insertHeaderCells: false
} :: JTableOpts


renderJTable :: JTableOpts -> Json -> Markup
renderJTable opt json = 
  opt.style.table $ table $ do 
    let t = sortTree opt.columnOrdering $ tFromJson [] json
    thead $ renderThead opt.style.tr opt.style.th t
    tbody $ renderTbody opt.style.tr opt.style.td t json

renderJTableArray :: JTableOpts -> [Json] -> Markup
renderJTableArray opt ja = renderJTable opt $ fromArray ja

renderJTableDef :: Json -> Markup
renderJTableDef = renderJTable defJTableOpts
