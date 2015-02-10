module Test.Main where

import Debug.Trace

import qualified Test.Data.Json.JTable.StrongCheck as SC


main = do
  trace "Running StrongCheck tests" 
  SC.main
