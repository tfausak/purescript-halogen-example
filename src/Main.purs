module Main where

import Prelude

import qualified Control.Monad.Aff as Aff
import qualified Halogen as Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.Util as Halogen

main = Aff.launchAff do
  { node: node } <- Halogen.runUI ui initialState
  Halogen.appendToBody node

data Query a = Toggle a

ui :: forall f. (Functor f) => Halogen.Component State Query f
ui = Halogen.component render eval

render :: Halogen.Render State Query
render state = H.div_
  [
    H.h1 [] [H.text "purescript-halogen-example"],
    H.button
      [E.onClick (E.input_ Toggle)]
      [H.text (if state.on then "On" else "Off")]
  ]

eval :: forall a. Halogen.Eval Query State Query a
eval (Toggle next) = do
  Halogen.modify (\ state -> state { on = not state.on })
  pure next

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }
