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


    data Cell where
      C :: JCursor -> Number -> Number -> JsonPrim -> Cell


    data Tree where
      T :: [String] -> Number -> Number -> [Tree] -> Tree


### Type Class Instances


    instance showCell :: Show Cell


    instance showTree :: Show Tree


### Values


    _nattr :: String -> Number -> Markup -> Markup


    cFromJson :: Tree -> JCursor -> Json -> [[Cell]]


    cMergeObj :: [[[Cell]]] -> [[Cell]]


    localeCompare :: String -> String -> Number


    mergeObjTuple :: Tree -> JCursor -> [Json] -> Maybe [[Cell]]


    renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup


    renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Json -> Markup


    renderThead :: (Markup -> Markup) -> (Tree -> Markup) -> Tree -> Markup


    sortTree :: ([String] -> [String] -> Ordering) -> Tree -> Tree


    strcmp :: String -> String -> Ordering


    tFromJson :: [String] -> Json -> Tree


    tMergeArray :: Tree -> Tree -> Tree


    tsToRows :: [Tree] -> [[Tree]]


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



