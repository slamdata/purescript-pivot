module Main where

import Prelude
import Data.Void
import Data.Tuple
import Data.Either
import Data.List (toList)

import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import qualified Control.Monad.Aff as Aff
import qualified Control.Monad.Aff.AVar as Aff
import Control.Plus

import Data.Argonaut.Core (Json(), jsonEmptyArray)
import Data.Argonaut.Parser (jsonParser)
import qualified Data.Json.JTable as J

import qualified Data.StrMap as StrMap

import Halogen
import Halogen.Util
import Halogen.Component

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

type ExampleState = String

data ExampleInput a
  = SetJsonText String a

parent :: forall g p. (Functor g) => ParentComponentP ExampleState Json ExampleInput J.JTableInput g Unit p
parent = component' render eval (\_ -> pure unit)
  where
    render :: Render ExampleState ExampleInput Unit
    render jsonString =
      H.div
        [ P.class_ $ H.className "container" ]
        [ H.h1_ [ H.text "purescript-jtable demo" ]
        , H.p_ [ H.text "Paste some JSON:" ]
        , H.p_
            [ H.textarea
                [ P.class_ $ H.className "form-control"
                , P.value jsonString
                , E.onValueInput $ E.input SetJsonText
                ]
            ]
        , H.h2_ [ H.text "Output" ]
        , H.slot unit
        ]

    eval :: EvalP ExampleInput ExampleState Json ExampleInput J.JTableInput g Unit p
    eval (SetJsonText jsonString next) = do
      query unit <<< action <<< J.SetJson $
        case jsonParser jsonString of
          Left _ -> jsonEmptyArray
          Right json -> json
      pure next

ui :: forall p g. (Plus g) => J.JTableOpts p -> InstalledComponent ExampleState Json ExampleInput J.JTableInput g Unit p
ui opts =
  install' parent \_ ->
    createChild (J.jtableComponent opts) jsonEmptyArray

type ExampleEffects =
  ( dom :: DOM.DOM
  , avar :: Aff.AVAR
  , err :: EXCEPTION
  )

main :: Eff ExampleEffects Unit
main =
  Aff.runAff throwException (const (pure unit)) $ do
    app <- runUI (ui J.jTableOptsDefault) (installedState "")
    appendToBody app.node
