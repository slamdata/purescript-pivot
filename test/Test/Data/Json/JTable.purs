module Test.Data.Json.JTable where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Data.Foldable (for_)
import Data.Argonaut.Core (Json(), JArray())
import Data.Argonaut.JCursor (primToJson, primNull)
import Data.Json.JTable (renderJTable, renderJTableDef, jTableOptsDefault)
import Data.Json.JTable.Internal (renderJTableRaw, Markup())
import Data.Void
import Test.StrongCheck
import qualified Halogen.HTML.Renderer.String as H
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

type TestEffects e =
  ( err :: EXCEPTION
  , random :: RANDOM
  , console :: CONSOLE
  | e
  )

jNull = primToJson primNull
foreign import j0 :: Json
foreign import jTup2 :: Json
foreign import jATup2 :: JArray
foreign import jTup3 :: Json
foreign import jATup3 :: JArray
foreign import jHomoTup2 :: Json
foreign import jAHomoTup2 :: JArray
foreign import jArr3 :: Json
foreign import jAArr3 :: JArray
foreign import jObj2 :: Json
foreign import jObjArr0 :: Json
foreign import jObj2Tup2 :: Json
foreign import jObj2Obj2 :: Json
foreign import jObjArr2Tup2 :: Json
foreign import jTup2Obj :: JArray
foreign import jObjTup2Obj :: Json
foreign import jObj2EArr :: Json
foreign import jObjWeird :: Json
foreign import jArrObj2Tups :: Json
foreign import jMergeObjTup :: Json


type TestCase =
  { json :: Json
  , html :: Markup Void
  , msg :: String
  }

cases :: Array TestCase
cases =
  [ { json: jNull
    , msg: "null"
    , html: H.table_ [ H.thead_ []
                     , H.tbody_ [ H.tr_ [ H.td_
                                            [ H.text ""]]]
                     ]
    }
  , { json: j0
    , msg: "0"
    , html: H.table_ [ H.thead_ []
                     , H.tbody_ [ H.tr_ [ H.td_
                                          [ H.text "0" ]]]
                     ]
    }
  , { json: jObj2
    , msg: "jObj2"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th_ [ H.text "a"]
                                        , H.th_ [ H.text "b"]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1"]
                                        , H.td_ [ H.text "two"]
                                        ]
                                ]
                     ]
    }
  , { json: jObj2Tup2
    , msg: "jObj2Tup2"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th_ [ H.text "a" ]
                                        , H.th [ P.colSpan 2 ] [ H.text "b"]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1" ]
                                        , H.td_ [ H.text "one" ]
                                        , H.td_ [ H.text "false"]
                                        ]
                                ]
                     ]
    }
  , { json: jObj2Obj2
    , msg: "jObj2Obj2"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th [ P.rowSpan 2 ] [ H.text "a"]
                                        , H.th [ P.colSpan 2 ] [ H.text "b"]
                                        ]
                                , H.tr_ [ H.th_ [ H.text "b1" ]
                                        , H.th_ [ H.text "b2" ]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1" ]
                                        , H.td_ [ H.text "one"]
                                        , H.td_ [ H.text "false" ]
                                        ]
                                ]
                     ]
    }
  , { json: jObjArr2Tup2
    , msg: "jObjArr2Tup2"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th [ P.colSpan 2 ] [H.text "a"]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1"]
                                        , H.td_ [ H.text "two"]
                                        ]
                                , H.tr_ [ H.td_ [ H.text "3" ]
                                        , H.td_ [ H.text "four" ]
                                        ]
                                ]
                     ]
    }
  , { json: jObjTup2Obj
    , msg: "jObjTup2Obj"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th [ P.colSpan 2 ] [ H.text "a" ]
                                        ]
                                , H.tr_ [ H.th_ [ H.text "x" ]
                                        , H.th_ [ H.text "y" ]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1" ]
                                       , H.td_ [ H.text "2" ]
                                       ]
                               ]
                     ]
    }
  , { json: jObj2EArr
    , msg: "jObj2EArr"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th_ [H.text "a"]
                                        , H.th_ [H.text "b"]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [H.text ""]
                                        , H.td_ [H.text "one"]
                                        ]
                                ]
                     ]
    }
  , { json: jObjWeird
    , msg: "jObjWeird"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th [ P.colSpan 5 ] [H.text "a"]
                                        , H.th [ P.colSpan 5
                                               , P.rowSpan 2 ] [H.text "b" ]
                                        ]
                                , H.tr_ [ H.th [ P.colSpan 4 ] [H.text "x" ]
                                        , H.th_ [ H.text "y" ]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1"]
                                        , H.td_ [ H.text "2"]
                                        , H.td_ [ H.text "3"]
                                        , H.td_ [ H.text "4"]
                                        , H.td_ [ H.text ""]
                                        , H.td_ [ H.text "1"]
                                        , H.td_ [ H.text "2"]
                                        , H.td_ [ H.text "3"]
                                        , H.td_ [ H.text "4"]
                                        , H.td_ [ H.text "5"]
                                        ]
                                ]
                     ]
    }
  , { json: jArrObj2Tups
    , msg: "jArrObj2Tups"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th [P.colSpan 3] [ H.text "a"]
                                        , H.th [P.colSpan 3] [ H.text "b"]
                                        ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "3" ]
                                        , H.td_ [ H.text "2" ]
                                        , H.td_ [ H.text "1" ]
                                        , H.td_ [ H.text "1" ]
                                        , H.td_ [ H.text "2" ]
                                        , H.td_ [ H.text "" ]
                                        ]
                                , H.tr_ [ H.td [P.colSpan 3] [ H.text ""]
                                        , H.td_ [H.text "3"]
                                        , H.td_ [H.text "2"]
                                        , H.td_ [H.text "1"]
                                        ]
                                ]
                     ]
    }
  , { json: jObjArr0
    , msg: "jObjArr0"
    , html: H.table_ [ H.thead_ [H.tr_ [H.th_ [H.text "a"]]]
                     , H.tbody_ [H.tr_ [H.td_ [H.text ""]]]]
    }
  , { json: jMergeObjTup
    , msg: "jMergeObjTup"
    , html: H.table_ [ H.thead_ [ H.tr_ [ H.th_ [ H.text "a" ]
                                        , H.th [P.rowSpan 2] [H.text "b"]
                                        ]
                                , H.tr_ [ H.th_ [ H.text "x" ] ]
                                ]
                     , H.tbody_ [ H.tr_ [ H.td_ [ H.text "0" ]
                                        , H.td_ [ H.text "8" ]
                                        ]
                                , H.tr_ [ H.td_ [ H.text "1" ]
                                        , H.td [P.rowSpan 2] [ H.text "9" ]
                                        ]
                                , H.tr_ [ H.td_ [ H.text "2" ]
                                        ]
                                ]
                     ]
    }
  ]

insertedCase :: TestCase
insertedCase =
  { json: jObj2Obj2
  , msg: "jObj2Obj2"
  , html: H.table_ [ H.thead_ [ H.tr_ [ H.th_ [ ]
                                      , H.th [P.colSpan 2] [ H.text "b" ]
                                      ]
                              , H.tr_ [ H.th_ [ H.text "a"]
                                      , H.th_ [ H.text "b1" ]
                                      , H.th_ [ H.text "b2" ]
                                      ]
                              ]
                   , H.tbody_ [ H.tr_ [ H.td_ [ H.text "1" ]
                                      , H.td_ [ H.text "one" ]
                                      , H.td_ [ H.text "false" ]
                                      ]
                              ]
                   ]
  }


assertion :: forall e. TestCase -> Eff (TestEffects e) Unit
assertion {json: json, msg: msg, html: html} = do
  let expected = H.renderHTML html
      actual = H.renderHTML $ renderJTableDef json
      errorMsg = msg <> "\nactual: " <> actual <> "\nexpected: " <> expected
  assert ((actual == expected) <?> errorMsg)


headerCellAssertion :: forall e. TestCase -> Eff (TestEffects e) Unit
headerCellAssertion {json: json, msg: msg, html: html} = do
  let expected = H.renderHTML html
      opts = jTableOptsDefault {insertHeaderCells = true}
      actual = H.renderHTML $ renderJTable opts json
      errorMsg = "special assertion" <> msg <> "\nactual: " <> actual <> "\nexpected: " <> expected
  assert ((actual == expected) <?> errorMsg)

main = do
  for_ cases assertion
  headerCellAssertion insertedCase

