module Data.Json.JTable.Internal
  ( renderJTableRaw
  , JTableOpts()
  , ColumnOrdering()
  , TableStyle()
  , JPath()
  , JTableQuery(..)
  , Markup()
  ) where

import Prelude (class Ord, Ordering, Unit, one, (-), zero, ($), (<$>), (==), flip, bind, (<<<), pure, (>>=), (>), (&&), not, (+), (>>>), (<=), const, return, (<))

import Control.Alt ((<|>))
import Control.MonadPlus (guard)

import Data.Argonaut.Core (Json())
import Data.Argonaut.Core as JSON
import Data.Argonaut.JCursor as JC
import Data.Array as A
import Data.Foldable (foldl, elem)
import Data.List as L
import Data.List (List(), (!!), toList, fromList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.StrMap as M
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Data.Void (Void())

import Halogen.HTML as H

max :: forall a. (Ord a) => a -> a -> a
max a b = if a > b then a else b

-- path of object keys, with array indices omitted
type JPath = List String

-- rows of cells
type Table = List (List Cell)

-- header data
type TreeRec =
  { label :: String
  , path :: JPath
  , width :: Int
  , height :: Int
  , children :: List Tree
  }

newtype Tree = Tree TreeRec

runTree :: Tree -> TreeRec
runTree (Tree t) = t

-- cell data
type CellRec =
  { cursor :: JC.JCursor
  , width :: Int
  , height :: Int
  , json :: JC.JsonPrim
  }

newtype Cell = Cell CellRec

runCell :: Cell -> CellRec
runCell (Cell c) = c

data JTableQuery a = SetJson Json a
type Markup f = H.HTML Void (f Unit)

type TableStyle =
  { table :: forall f. Array (Markup f) -> Markup f
  , tr    :: forall f. Array (Markup f) -> Markup f
  , th    :: forall f. String -> JPath -> Int -> Int -> Markup f
  , td    :: forall f. JC.JCursor -> JC.JsonPrim -> Int -> Int -> Markup f
  }

type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering

type JTableOpts =
  { style :: TableStyle
  , columnOrdering :: ColumnOrdering
  , insertHeaderCells :: Boolean
  , maxTupleSize :: Int
  }

foldJsonPrim :: forall a. (JC.JsonPrim -> a) -> (JSON.JArray -> a) -> (JSON.JObject -> a) -> Json -> a
foldJsonPrim f =
  JSON.foldJson (\_-> f JC.primNull) (f <<< JC.primBool) (f <<< JC.primNum) (f <<< JC.primStr)

toPrim :: Json -> Maybe JC.JsonPrim
toPrim = foldJsonPrim Just (const Nothing) (const Nothing)

enumerate :: forall a. List a -> List (Tuple a Int)
enumerate xs = L.zipWith Tuple xs (L.range zero $ (L.length xs - one))

renderJTableRaw :: forall f. JTableOpts -> Json -> Markup f
renderJTableRaw o json =
  o.style.table
    [ renderThead o.style.tr o.style.th padded
    , renderTBody o.style.tr o.style.td sorted table
    ]
  where
    padded :: Tree
    padded =
      if o.insertHeaderCells
      then (padTree (runTree sorted).height) $ sorted
      else sorted

    sorted :: Tree
    sorted = sortTree o.columnOrdering raw

    raw :: Tree
    raw = treeFromJson o.maxTupleSize "" L.Nil json

    table :: Table
    table = mkTable o.maxTupleSize sorted JC.JCursorTop json


padTree :: Int -> Tree -> Tree
padTree maxh tree@(Tree t) =
  if not $ L.null t.children
  then Tree $ t { children = padTree (maxh - one) <$> t.children }
  else
    if maxh < 1
    then Tree $ t{ height = one }
    else Tree $ t{ label = mempty
                 , height = one
                 , children = L.singleton $ padTree (maxh - one) (Tree t)
                 }

sortTree :: ColumnOrdering -> Tree -> Tree
sortTree ord (Tree t) = Tree $ t {children = L.sortBy sortFn $ (sortTree ord <$> t.children)}
  where
    sortFn :: Tree -> Tree -> Ordering
    sortFn (Tree t1) (Tree t2) =
      ord t1.label t1.path t2.label t2.path

renderRows
  :: forall a f
   . (Array (Markup f) -> Markup f)
  -> (Int -> Int -> a -> Markup f)
  -> List (List a)
  -> Array (Markup f)
renderRows tr cellf rows = fromList do
  Tuple row y <- fromList $ enumerate rows
  return $ tr do
    Tuple cell x <- fromList $ enumerate row
    return $ cellf y x cell

tablesToRows :: List Tree -> List (List Tree)
tablesToRows ts =
  if L.null ts
  then L.Nil
  else L.Cons ts (tablesToRows (ts >>= runTree >>> _.children))

renderThead
  :: forall f
   . (Array (Markup f) -> Markup f)
  -> (String -> JPath -> Int -> Int -> Markup f)
  -> Tree
  -> Markup f
renderThead tr thf (Tree t) = H.thead_ $ renderRows tr tdf' $ tablesToRows t.children
  where
    tdf' :: Int -> Int -> Tree -> Markup f
    tdf' y x (Tree t) = thf t.label t.path t.width (height y t.children)

    height :: Int -> List Tree -> Int
    height i k =
      if L.null k
      then t.height - i
      else one

renderTBody
  :: forall f
   . (Array (Markup f) -> Markup f)
  -> (JC.JCursor -> JC.JsonPrim -> Int -> Int -> Markup f)
  -> Tree
  -> Table
  -> Markup f
renderTBody tr tdf (Tree t) table = H.tbody_ (renderRows tr tdf' table)
  where
    tdf' :: Int -> Int -> Cell -> Markup f
    tdf' _ _ (Cell c) = tdf c.cursor c.json c.width c.height

treeFromJson :: Int -> String -> JPath -> Json -> Tree
treeFromJson maxTupleSize label path = foldJsonPrim (const prim) array obj
  where
    tree :: TreeRec
    tree = { label: label
           , path: path
           , width: one
           , height: zero
           , children: mempty
           }

    prim :: Tree
    prim = Tree tree

    array :: JSON.JArray -> Tree
    array ja = fromMaybe (jarr ja) $ tuple ja

    tuple :: JSON.JArray -> Maybe Tree
    tuple ja =
      Tree <<< tree { width = _ } <$>
      widthOfPrimTuple maxTupleSize path ja

    jarr :: JSON.JArray -> Tree
    jarr ja = Tree $ tree { width = width, height = t.height, children = t.children }
      where
        width :: Int
        width = max t.width childrenWidth

        childrenWidth :: Int
        childrenWidth =
          case (L.nub $ (runTree >>> _.width) <$> ts) of
            L.Cons tsw L.Nil -> tsw
            _ -> one


        t :: TreeRec
        t = runTree $ foldl mergeTrees (Tree $ tree {height = zero}) $ ts >>= runTree >>> _.children

        ts :: List Tree
        ts = toList $ treeFromJson maxTupleSize label path <$> ja


    obj :: JSON.JObject -> Tree
    obj jo =
      if M.isEmpty jo
      then Tree tree
      else Tree $ tree {width = width, height = height, children = children }

      where
        assocToTree :: JSON.JAssoc -> Tree
        assocToTree (Tuple l j) =
          treeFromJson maxTupleSize l (L.snoc path l) j

        children :: List Tree
        children = assocToTree <$> M.toList jo

        width :: Int
        width = foldl (+) zero $ (runTree >>> _.width) <$> children

        height :: Int
        height = one + (foldl (+) zero $ (runTree >>> _.height) <$> children)

widthOfPrimTuple :: Int -> JPath -> JSON.JArray -> Maybe Int
widthOfPrimTuple maxTupleSize path ja = do
  L.head path
  ja A.!! 1
  for ja toPrim
  let l = A.length ja
  guard $ l <= maxTupleSize
  pure l


mergeTrees :: Tree -> Tree -> Tree
mergeTrees (Tree t) (Tree nt) = maybe notChild go $ i >>= \ix -> t.children !! ix
  where
    i :: Maybe Int
    i = L.findIndex (runTree >>> _.label >>> \x -> x == nt.label) t.children

    notChild :: Tree
    notChild = Tree $ t { width = width, height = height, children = children}
      where
        width :: Int
        width =
          if L.null t.children
          then nt.width
          else t.width + nt.width

        height :: Int
        height = max t.height (nt.height + one)

        children :: List Tree
        children = L.snoc t.children (Tree nt)

    go :: Tree -> Tree
    go c@(Tree child) = Tree child { width = width, height = height, children = children }
      where
        width :: Int
        width = t.width - child.width + maxDescendantWidth

        height :: Int
        height = max t.height (subChild.height + one)

        children :: List Tree
        children =
          fromMaybe t.children $ do
            ix <- i
            L.updateAt ix (Tree child { width = maxDescendantWidth, height = subChild.height, children = subChild.children }) t.children

        subChild :: TreeRec
        subChild = runTree $ foldl mergeTrees c nt.children

        maxDescendantWidth :: Int
        maxDescendantWidth =
          max subChild.width
            if L.null nt.children && not (L.null subChild.children)
            then one
            else nt.width


mkTable :: Int -> Tree -> JC.JCursor -> Json -> Table
mkTable maxTupleSize (Tree t) c = foldJsonPrim prim array obj
  where
    width :: JSON.JArray -> Maybe Int
    width ja =
      if L.null t.children && t.width > 1
      then widthOfPrimTuple maxTupleSize t.path ja
      else Nothing

    prim :: JC.JsonPrim -> Table
    prim jp = L.singleton $ L.singleton $ Cell { cursor: c, width: t.width, height: one, json: jp }

    array :: JSON.JArray -> Table
    array ja = fromMaybe (jarr ja) $ tuple ja

    tuple :: JSON.JArray -> Maybe Table
    tuple ja = (primtup ja) <|> (objtup ja)

    primtup :: JSON.JArray -> Maybe Table
    primtup ja = do
      width ja
      pure $ L.singleton $ mkCells <$> (L.range zero (t.width - one))
      where
        mkCells :: Int -> Cell
        mkCells i =
          Cell { cursor: JC.downIndex i c
               , width: one
               , height: one
               , json: fromMaybe JC.primNull (ja A.!! i >>= toPrim)
               }

    objtup :: JSON.JArray -> Maybe Table
    objtup ja = mergeObjTuple maxTupleSize (Tree t) c ja

    jarr :: JSON.JArray -> Table
    jarr ja = do
      Tuple j i <- enumerate $ toList ja
      mkTable maxTupleSize (Tree t) (JC.downIndex i c) j

    obj :: JSON.JObject -> Table
    obj jo =
      if M.isEmpty jo
      then L.singleton $ L.singleton $ Cell { cursor: c, width: t.width, height: one, json: JC.primNull }
      else mergeTableTuples $ labeledTable <$> t.children
      where
        labeledTable :: Tree -> Tuple Int Table
        labeledTable (Tree tree) =
          let j = fromMaybe (JC.primToJson JC.primNull) $ M.lookup tree.label jo in
          Tuple tree.width $ mkTable maxTupleSize (Tree tree) (JC.downField tree.label c) j

mergeObjTuple :: Int -> Tree -> JC.JCursor -> JSON.JArray -> Maybe Table
mergeObjTuple maxTupleSize (Tree t) c ja = do
  A.head ja
  jos <- for ja JSON.toObject
  let
    kss :: Array (Array String)
    kss = M.keys <$> jos
    ks :: Array String
    ks = A.concat kss

  guard $ A.length ks == A.length (A.nub ks)

  let
    mkTableTuple :: Tree -> Maybe (Tuple Int Table)
    mkTableTuple (Tree tree) = do
      i <- A.findIndex (elem tree.label) kss
      obj <- jos A.!! i
      let j = fromMaybe (JC.primToJson JC.primNull) $ M.lookup tree.label obj
      pure $ Tuple tree.width $ mkTable maxTupleSize (Tree tree) (JC.downField tree.label $ JC.downIndex i c) j

  pure $ mergeTableTuples $ L.catMaybes $ mkTableTuple <$> t.children


mergeTableTuples :: List (Tuple Int Table) -> Table
mergeTableTuples tables =
  oneColumn <$> (L.range zero $ max zero $ maxh - one)

  where
    maxh :: Int
    maxh = foldl max 0 $ (L.length <<< snd) <$> tables

    oneColumn :: Int -> List Cell
    oneColumn n = do
      Tuple width table <- tables
      let rnOr = flip fromMaybe (table !! n)
      case table of
        L.Cons r L.Nil ->
          if n == 0
          then (\(Cell c) -> Cell $ c {height = maxh}) <$> r
          else rnOr L.Nil
        _ ->
          rnOr $ L.singleton $ Cell
            { cursor: JC.JCursorTop
            , width: width
            , height: one
            , json: JC.primNull
            }
