module Render.Dom.JTable where

import Data.Argonaut
import Data.Either
import Text.Smolder.Markup

data JSemantic = Integral
               | Fractional
               | Date
               | DateTime
               | Time
               | Interval
               | Text
               | Bool
               | Percent
               | Currency
               | NA

type Level = Number

-- JPath??

-- data ColumnOrdering = InOrdering | CustomOrdering (JPath -> JPath -> Ordering)

data TableStyle =
TableStyle {
  table   :: Level -> Markup -> Markup,
  cell    :: JSemantic -> Markup -> Markup,
--  head    :: JPath -> Markup -> Markup,
  row     :: Markup -> Markup }
