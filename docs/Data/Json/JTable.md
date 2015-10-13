## Module Data.Json.JTable

#### `noStyle`

``` purescript
noStyle :: forall p. TableStyle p
```

#### `bootstrapStyle`

``` purescript
bootstrapStyle :: forall p. TableStyle p
```

#### `debugStyle`

``` purescript
debugStyle :: forall p. TableStyle p
```

#### `inOrdering`

``` purescript
inOrdering :: ColumnOrdering
```

#### `alphaOrdering`

``` purescript
alphaOrdering :: ColumnOrdering
```

#### `jTableOptsDefault`

``` purescript
jTableOptsDefault :: forall p. JTableOpts p
```

#### `renderJTable`

``` purescript
renderJTable :: forall p. JTableOpts p -> Json -> Markup p
```

#### `renderJTableDef`

``` purescript
renderJTableDef :: forall p a. Json -> Markup p
```

#### `jtableComponent`

``` purescript
jtableComponent :: forall p g. JTableOpts p -> Component Json JTableInput g p
```


