module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  , jtableComponent
  , module I
  ) where

import Prelude
import Data.String (joinWith)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), runJsonPrim)
import Data.Const
import Data.Void
import Data.Json.JSemantic (toSemanticDef, JSemantic(..), renderJSemantic)
import Data.List (fromList)

import Data.Json.JTable.Internal
import qualified Data.Json.JTable.Internal (JTableInput(..), JTableOpts()) as I

import qualified Halogen (modify) as H
import qualified Halogen.Component as H
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

renderJsonSimple :: JsonPrim -> String
renderJsonSimple j = renderJSemantic $ toSemanticDef j

spans :: forall p r. Int -> Int -> Array (P.IProp (colSpan :: P.I, rowSpan :: P.I | r) p)
spans w h =
  (if w > 1 then [ P.colSpan w ] else [])
    <> (if h > 1 then [ P.rowSpan h ] else [])

noStyle :: forall p. TableStyle p
noStyle =
  { table : H.table_
  , tr : H.tr_
  , th : \l _ w h -> H.th (spans w h) [ H.text l ]
  , td : \_ j w h -> H.td (spans w h) [ H.text $ renderJsonSimple j ]
  }

bootstrapStyle :: forall p. TableStyle p
bootstrapStyle = noStyle { table = H.table [ P.class_ (H.className "table") ] }

debugStyle :: forall p. TableStyle p
debugStyle = noStyle
  { th = \_ p w h -> H.th (spans w h) [ H.text $ joinWith "." $ fromList p ]
  , td = \c j w h ->
           H.td
             (spans w h)
             [ H.small
                 [ P.class_ (H.className "grey") ]
                 [ H.text (show c) ]
             , H.br_
             , H.text $ show j
             ]
  }

inOrdering :: ColumnOrdering
inOrdering _ _ _ _ = EQ

alphaOrdering :: ColumnOrdering
alphaOrdering l1 _ l2 _ = compare l1 l2

jTableOptsDefault :: forall p. JTableOpts p
jTableOptsDefault =
  { style: noStyle
  , columnOrdering: inOrdering
  , insertHeaderCells: false
  , maxTupleSize: 10
  }

renderJTable :: forall p. JTableOpts p -> Json -> Markup p
renderJTable = renderJTableRaw

renderJTableDef :: forall p a. Json -> Markup p
renderJTableDef = renderJTable jTableOptsDefault

jtableComponent :: forall p g. JTableOpts p -> H.Component Json JTableInput g p
jtableComponent opts = H.component render eval
  where
    render :: H.Render Json JTableInput _
    render json = renderJTable opts json

    eval :: H.Eval JTableInput Json JTableInput g
    eval (SetJson json next) = do
      H.modify \_ -> json
      pure next
