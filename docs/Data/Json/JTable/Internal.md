## Module Data.Json.JTable.Internal

#### `JPath`

``` purescript
type JPath = List String
```

#### `JTableInput`

``` purescript
data JTableInput a
  = SetJson Json a
```

#### `Markup`

``` purescript
type Markup p = HTML p (JTableInput Unit)
```

#### `TableStyle`

``` purescript
type TableStyle p = { table :: Array (Markup p) -> Markup p, tr :: Array (Markup p) -> Markup p, th :: String -> JPath -> Int -> Int -> Markup p, td :: JCursor -> JsonPrim -> Int -> Int -> Markup p }
```

#### `ColumnOrdering`

``` purescript
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering
```

#### `JTableOpts`

``` purescript
type JTableOpts p = { style :: TableStyle p, columnOrdering :: ColumnOrdering, insertHeaderCells :: Boolean, maxTupleSize :: Int }
```

#### `renderJTableRaw`

``` purescript
renderJTableRaw :: forall p a. JTableOpts p -> Json -> Markup p
```


