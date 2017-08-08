module BlockViewer where

import Prelude
import Structures
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr, all)
import Data.Array ((..),concatMap, zip, filter, null, zipWith, length, reverse, snoc, toUnfoldable)
import Debug.Trace
import Data.String as Str
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console
import DOM (DOM)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HCore

type State = {
  structure :: Maybe Block',
  dragSource :: Maybe (Array Int)
}

data BlockView = BlockView (Array Int) Block' | InputBlock (Array Int) Type

data Query a
  = IsEmpty (Boolean -> a)
  | Remove (Array Int) a
  | StartDrag (Array Int) DragEvent a
  | Clear a
  -- | Increase a
  -- | GetVal (Int -> a)

data Message
  = New Block'
  | Updated Block'
  | Removed

myBlockViewer :: forall eff. H.Component HH.HTML Query Unit Message (Aff (dom :: DOM | eff))
myBlockViewer =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {
    structure: Just $ Block' addBlock [Just (intBlock' 1),Just (Block' addBlock [Just (intBlock' 1),{-Just (intBlock' 1)-}Nothing])],
    dragSource: Nothing
  }
  
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_ $
      [ HH.div_ [HH.text $ maybe "" show evaluated ] ]
      <> structureRendering structure
    where
      structure = state.structure
      evaluated :: Maybe Val
      evaluated = evaluate =<< structure
  
  structureRendering :: Maybe Block' -> Array (H.ComponentHTML Query)
  structureRendering (Just block'@(Block' (Block name fn output inputs) connected)) =
    [ HH.div [classes ["structureView"]] [ structureToDiv (BlockView [] block') ]
    ]
  structureRendering Nothing =
    [ HH.p_ [ HH.text ("Nothing") ]
    ]
  
  structureToDiv :: BlockView -> H.ComponentHTML Query
  structureToDiv (InputBlock loc typ) = HH.div [classes ["input", toClass typ]] []
  structureToDiv blockView@(BlockView loc (Block' (Block name fn out ins) connected)) =
    HH.div [ HP.draggable true, classes ["block", toClass out, "void" `if_` null connected], HE.onDragStart (HE.input (StartDrag loc)) ]
      (
        [ HH.div [HE.onClick (HE.input_ (Remove loc)), classes ["block-text"]]
          [ HH.text (name <> " \x2237 " <> foldr (<>) "" (map (\input -> show input <> " \x2192 ") ins) <> show out) ]
        ]
        <> [ HH.div [classes ["block-inputs"]] $ map structureToDiv (getConnected blockView)] `if_` not (null connected)
      )
  
  getConnected :: BlockView -> Array BlockView
  getConnected (InputBlock loc _) = []
  getConnected (BlockView loc (Block' _ [])) = []
  getConnected (BlockView loc (Block' (Block _ _ _ ins) connected)) =
    zipWith (maybeToBlockView loc) (0 .. length connected) $ zip ins connected
  
  maybeToBlockView :: Array Int -> Int -> (Tuple Type (Maybe Block')) -> BlockView
  maybeToBlockView loc i (Tuple typ Nothing) = InputBlock (loc `snoc` i) typ
  maybeToBlockView loc i (Tuple typ (Just block')) = BlockView (loc `snoc` i) block'
  
  classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
  classes = HP.classes <<< map HCore.ClassName <<< filter (not <<< Str.null)
  
  toClass :: Type -> String
  toClass IntType = "int"
  toClass ColourType = "colour"
  
  if_ :: forall a . Monoid a => a -> Boolean -> a
  if_ x bool = if bool then x else mempty

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | eff))
  eval = case _ of
    -- Negate next -> do
    --   state <- H.get
    --   let nextState = - state
    --   H.put nextState
    --   H.raise $ Changed nextState
    --   pure next
    -- Increase next -> do
    --   state <- H.get
    --   let nextState = state + 1
    --   H.put nextState
    --   H.raise $ Changed nextState
    --   pure next
    Remove loc reply -> do
      state <- H.get
      let nextState = state { structure = removeBlock (toUnfoldable loc) state.structure }
      H.put nextState
      pure reply
    StartDrag loc ev reply -> do
      state <- H.get
      H.liftEff $ stopPropagation (dragEventToEvent ev)
      let nextState = state { dragSource = Just (trace (show loc) (const loc)) }
      H.put nextState
      pure reply
    Clear reply -> do
      H.put {structure: Nothing, dragSource: Nothing}
      pure reply
    IsEmpty reply -> do
      state <- H.get
      pure (reply (isJust state.structure))
