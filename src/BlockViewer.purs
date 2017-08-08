module BlockViewer where

import Prelude
import Structures
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr)
import Data.Array (filter, length, null, snoc, toUnfoldable, zip, zipWith, (..))
import Debug.Trace
import Data.String as Str
import Control.MonadZero (guard)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console
import DOM (DOM)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation, preventDefault)
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
  | Drag (Array Int) DragEvent a
  | Drop (Array Int) a
  | DragEnd a
  | PreventDefault DragEvent a
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
      <> structureRendering state
    where
      evaluated :: Maybe Val
      evaluated = evaluate =<< state.structure
  
  structureRendering :: State -> Array (H.ComponentHTML Query)
  structureRendering state = case state.structure of
    Nothing ->
      [ HH.p_ [ HH.text ("Nothing") ]
      ]
    Just block' ->
      [ HH.div [ HE.onDragEnd (HE.input_ DragEnd), classes ["structureView"]] [ structureToDiv state (BlockView [] block') ]
      ]
  
  structureToDiv :: State -> BlockView -> H.ComponentHTML Query
  structureToDiv _ (InputBlock loc typ) =
    HH.div  [ classes ["input", toClass typ]
            , HE.onDragOver (HE.input PreventDefault)
            , HE.onDrop (HE.input_ (Drop loc))
            ] []
  structureToDiv state blockView@(BlockView loc (Block' (Block name fn out ins) connected)) =
    HH.div  [ HP.draggable true,
              classes ["block", toClass out, "void" `if_` null connected, "dragged" `if_` (state.dragSource == Just loc)],
              HE.onDragStart (HE.input (Drag loc))
            ]
      (
        [ HH.div [HE.onClick (HE.input_ (Remove loc)), classes ["block-text"]]
          [ HH.text (name <> " \x2237 " <> foldr (<>) "" (map (\input -> show input <> " \x2192 ") ins) <> show out) ]
        ]
        <> [ HH.div [classes ["block-inputs"]] $ map (structureToDiv state) (getConnected blockView)] `if_` not (null connected)
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
    Remove loc reply -> do
      state <- H.get
      let nextState = state { structure = removeBlock (toUnfoldable loc) state.structure }
      H.put nextState
      pure reply
    Drag loc ev reply -> do
      state <- H.get
      H.liftEff $ stopPropagation (dragEventToEvent ev)
      let nextState = state { dragSource = Just loc }
      H.put nextState
      pure reply
    DragEnd reply -> do
      state <- H.get
      let nextState = state { dragSource = Nothing }
      H.put nextState
      pure reply
    PreventDefault ev reply -> do
      H.liftEff $ preventDefault (dragEventToEvent ev)
      pure reply
    Drop loc reply -> do
      state <- H.get
      let structure' = fromMaybe state.structure $ do
            source <- toUnfoldable <$> state.dragSource
            structure <- state.structure
            movedBlock <- findBlock source state.structure
            let srcType = getRet movedBlock
            let dest = toUnfoldable loc
            destType <- getType dest =<< state.structure
            guard $ srcType == destType
            pure (insertBlock dest movedBlock $ removeBlock source (Just structure))
      
      H.put {structure: structure', dragSource: Nothing}
      pure reply
    IsEmpty reply -> do
      state <- H.get
      pure (reply (isJust state.structure))
