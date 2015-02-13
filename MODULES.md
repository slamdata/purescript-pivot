# Module Documentation

## Module Data.Json.JTable

### Types


    type ColumnOrdering = [String] -> [String] -> Ordering


    type JTableOpts = { insertHeaderCells :: Boolean, columnOrdering :: ColumnOrdering, style :: TableStyle }


    type TableStyle = { td :: JCursor -> JsonPrim -> Markup, th :: [String] -> Markup, tr :: Markup -> Markup, table :: Markup -> Markup }


### Values


    renderJTable :: JTableOpts -> Json -> Markup


    renderJTableArray :: JTableOpts -> [Json] -> Markup


    renderJTableDef :: Json -> Markup


## Module Data.Json.JTable.Internal

### Types

     cell data

    data Cell where
      C :: JCursor -> Number -> Number -> JsonPrim -> Cell

     header data

    data Tree where
      T :: [String] -> Number -> Number -> [Tree] -> Tree


### Type Class Instances


    instance showCell :: Show Cell


    instance showTree :: Show Tree


### Values


    _nattr :: String -> Number -> Markup -> Markup

     produce data table from json, according to header tree

    cFromJson :: Tree -> JCursor -> Json -> [[Cell]]

     merge table segments for each key of an object into one

    cMergeObj :: [[[Cell]]] -> [[Cell]]

     pad tall header cells from above

    insertHeaderCells :: Number -> Tree -> Tree


    localeCompare :: String -> String -> Number

     maybe merge a tuple of objects into a table segment

    mergeObjTuple :: Tree -> JCursor -> [Json] -> Maybe [[Cell]]

     render a grid from an array of arrays

    renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup


    renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Json -> Markup


    renderThead :: (Markup -> Markup) -> (Tree -> Markup) -> Tree -> Markup

     sort header tree by ColumnOrdering

    sortTree :: ([String] -> [String] -> Ordering) -> Tree -> Tree


    strcmp :: String -> String -> Ordering

     produce a tree of header data from json

    tFromJson :: [String] -> Json -> Tree

     add child to tree, unify if exists

    tMergeArray :: Tree -> Tree -> Tree

     produce header rows from header tree

    tsToRows :: [Tree] -> [[Tree]]

     maybe return the width of a tuple composed of primitive values

    widthOfPrimTuple :: [String] -> [Json] -> Maybe Number


## Module Data.Json.JSemantic

### Types


    data JSemantic where
      Integral :: Number -> JSemantic
      Fractional :: Number -> JSemantic
      Date :: Date.Date -> JSemantic
      DateTime :: Date.Date -> JSemantic
      Time :: Date.Date -> JSemantic
      Interval :: Date.Date -> Date.Date -> JSemantic
      Text :: String -> JSemantic
      Bool :: Boolean -> JSemantic
      Percent :: Number -> JSemantic
      Currency :: Number -> JSemantic
      NA :: JSemantic


### Type Class Instances


    instance showSemantic :: Show JSemantic


### Values


    toSemantic :: JsonPrim -> JSemantic



