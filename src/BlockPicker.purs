module BlockPicker where

import Prelude
import Structures
import Helpers

import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_, for_)
import Data.Array ((!!), length, zipWith, snoc, (..))
import Control.Monad.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (prompt)
import DOM.HTML.Types (PROMPT)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Types (Event)
import DOM.Event.Event (stopPropagation, preventDefault)

type State = {
  blocks :: Array Block,
  dragSource :: Maybe Int,
  dragTarget :: Maybe DragTarget
}

data DragTarget = Add | Delete

data Query a
  = Drag Int DragEvent a
  | DragEnd a
  | GetDragged (Maybe Block' -> a)
  | DragOver DragEvent a
  | Drop DragTarget a
  | DropBlock Block' (Boolean -> a)

myBlockPicker :: forall eff. H.Component HH.HTML Query Unit Message (Aff (prompt :: PROMPT, dom :: DOM | eff))
myBlockPicker =
  H.component
    { initialState: const { blocks, dragSource: Nothing, dragTarget: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [classes ["picker"]]
      [ HH.h1_ [ HH.text "Blocks.purs" ]
      , HH.ul_ $ zipWith menuItem (0 .. length state.blocks) state.blocks
      , HH.div [HE.onDragOver (HE.input DragOver), HE.onDrop (HE.input_ $ Drop Add), classes ["add"]] [HH.text "+"]
      , HH.div [HE.onDragOver (HE.input DragOver), HE.onDrop (HE.input_ $ Drop Delete), classes ["delete"]] [HH.text "—"]
      ]
  
  menuItem :: Int -> Block -> H.ComponentHTML Query
  menuItem i block@(Block name fn out ins) =
    HH.li
        [ HE.onDragStart (HE.input (Drag i))
        , HE.onDragEnd (HE.input_ DragEnd)
        , HP.draggable true
        ]
      [ HH.text $ name <> typeSignature block
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (prompt :: PROMPT, dom :: DOM | eff))
  eval = case _ of
    Drag i ev reply -> do
      state <- H.get
      H.liftEff $ stopPropagation (dragEventToEvent ev)
      H.liftEff $ setEffectAllowed "move" ev
      let valid = i < length state.blocks
      H.put $ state { dragSource = (if valid then Just i else Nothing) }
      when valid $ H.raise SetDrag
      pure reply
    DragEnd reply -> do
      state <- H.get
      let nextState = state { dragSource = Nothing }
      H.put nextState
      H.raise StopDrag
      pure reply
    GetDragged reply -> do
      state :: State <- H.get
      pure $ reply do
        dragSource :: Int <- state.dragSource
        block :: Block <- state.blocks !! dragSource
        pure $ initBlock block
    DragOver ev reply -> do
      H.liftEff $ preventDefault (dragEventToEvent ev)
      H.liftEff $ setDropEffect "move" ev
      pure reply
    Drop target reply -> do
      state <- H.get
      H.put $ state {dragTarget = Just target}
      H.raise $ HandleDrop
      pure reply
    DropBlock block' reply -> do
      state <- H.get
      case state.dragTarget of
        Nothing -> pure $ reply false
        Just Delete -> do
          H.put $ state {dragTarget = Nothing}
          pure $ reply true
        Just Add -> do
          mName <- H.liftEff $ window >>= prompt ("Choose a name for your new block" <> typeSignature (getBlock $ joinBlocks "" block'))
          for_ mName $ \name -> do
            let newBlock' = getBlock $ joinBlocks name block'
            H.put $ state {blocks = state.blocks `snoc` newBlock'}
          pure $ reply false

blocks :: Array Block
blocks =
  [ addBlock
  , redBlock
  , greenBlock
  , blueBlock
  , rgbBlock
  , idBlock IntType
  , idBlock ColourType
  , colourBlock  0 0 0
  , valBlock 0
  ]