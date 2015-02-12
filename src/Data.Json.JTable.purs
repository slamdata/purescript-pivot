module Data.Json.JTable
  ( renderJTable, renderJTableArray, renderJTableDef
  , JTableOpts(..), defJTableOpts
  , ColumnOrdering(..), inOrdering, alphaOrdering
  , TableStyle(..), noStyle, bootstrapStyle, debugStyle
  ) where

import Data.Json.JTable.Internal

import Data.String (joinWith)
import qualified Data.Array.Unsafe as AU
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Text.Smolder.HTML (table, thead, tbody, tr, th, td, br, small)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup(..), MarkupM(..), Attributable, attribute, (!), text)
import Data.Foldable (mconcat)


type TableStyle = {
  table :: Markup -> Markup,
  tr    :: Markup -> Markup ,
  th    :: [String] -> Markup,
  td    :: JCursor -> JsonPrim -> Markup }

renderJsonSimple j = runJsonPrim j (const "&nbsp;") show show id

noStyle = {
  table: table, 
  tr: tr, 
  th: \p -> th $ text $ AU.last p,
  td: \c j -> td $ text $ renderJsonSimple j
    
} :: TableStyle

bootstrapStyle = (noStyle {
  table = \m -> table ! attribute "class" "table" $ m}) :: TableStyle

renderJsonSemantic :: JCursor -> JsonPrim -> String
renderJsonSemantic = \c j -> show j

semanticStyle = (noStyle {
  td = \c j -> td $ text $ renderJsonSemantic c j} :: TableStyle)

debugStyle = (noStyle {
  th = (\p -> th $ text $ joinWith "." p),
  td = (\c j -> td $ mconcat $
    [(small ! className "grey" $ text $ show c), (br), (text $ show j)]
)}::TableStyle)


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
renderJTable o = renderJTableRaw o { style = o.style {
  th = (\t -> o.style.th (t # tPath)),
  td = (\c -> o.style.td (c # cCursor) (c # cJsonPrim)) }}

renderJTableArray :: JTableOpts -> [Json] -> Markup
renderJTableArray opt ja = renderJTable opt $ fromArray ja

renderJTableDef :: Json -> Markup
renderJTableDef = renderJTable defJTableOpts
