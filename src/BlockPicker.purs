module BlockPicker where

import Prelude
import Structures
import Helpers

import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Data.Array ((!!), length)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = {
  blocks :: Array Block,
  chosen :: Maybe Int
}

data Query a
  = Drag Int a

myBlockPicker :: forall m. H.Component HH.HTML Query Unit Message m
myBlockPicker =
  H.component
    { initialState: const { blocks, chosen: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [classes ["picker"]]
      [ HH.h1_ [ HH.text "Blocks.purs" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Drag i reply -> do
      state <- H.get
      let valid = i < length state.blocks
      H.put $ state { chosen = (if valid then Just i else Nothing) }
      when valid $ H.raise SetDrag
      pure reply

blocks :: Array Block
blocks =
  [ addBlock
  , redBlock
  , greenBlock
  , blueBlock
  , idBlock IntType
  , idBlock ColourType
  , colourBlock  0 0 0
  , valBlock 0
  ]