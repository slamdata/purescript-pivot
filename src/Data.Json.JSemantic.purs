module Data.Json.JSemantic
  ( JSemantic(..)
  , toSemantic
  ) where

import Data.Argonaut.JCursor(JsonPrim(..), runJsonPrim)
import Data.Maybe
import Data.String.Regex
import qualified Data.Date as Date
import Math


data JSemantic = Integral   Number
               | Fractional Number
               | Date       Date.Date
               | DateTime   Date.Date
               | Time       Date.Date
               | Interval   Date.Date Date.Date
               | Text       String
               | Bool       Boolean
               | Percent    Number
               | Currency   Number
               | NA


instance showSemantic :: Show JSemantic where
  show (Integral   n) = show n
  show (Fractional n) = show n
  show (Date       d) = show d
  show (DateTime   d) = show d
  show (Time       d) = show d
  show (Interval   d1 d2) = show d1 ++ " - " ++ show d2
  show (Text       s) = s
  show (Bool       b) = show b
  show (Percent    n) = show n ++ "%"
  show (Currency   n) = "$" ++ show n
  show (NA)           = ""


noFlags :: RegexFlags
noFlags  = { global     : false
           , ignoreCase : false
           , multiline  : false
           , sticky     : false
           , unicode    : false }

percentRegex :: String -> Boolean
percentRegex = test $ regex "^\\d+[%]$" noFlags

currencyRegex :: String -> Boolean
currencyRegex = test $ regex "^\\$?([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?$" noFlags

analyzeNum :: Number -> JSemantic
analyzeNum n | floor n == n = Integral n
analyzeNum n                = Fractional n

foreign import parsePercent """function parsePercent (s) {
  return o.replace("%", "") * 1}""" :: String -> Number

foreign import parseCurrency """function parseCurrency (s) {
  return s.replace(",", "").replace("$", "") * 1}""" :: String -> Number


analyzeStr :: String -> JSemantic
analyzeStr s = case Date.fromString s of
  Nothing ->  if percentRegex s then Percent $ parsePercent s
              else if currencyRegex s then Currency $ parseCurrency s
              else Text s
  (Just d) ->  DateTime d

toSemantic :: JsonPrim -> JSemantic
toSemantic p = runJsonPrim p (const NA) Bool analyzeNum analyzeStr
