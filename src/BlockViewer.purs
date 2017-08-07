module BlockViewer where

import Prelude
import Structures
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr, all)
import Data.Array (concatMap, zip)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HCore

type State = Maybe Block'

data BlockView = BlockView Block' | InputBlock Type | VoidBlock

data Query a
  = IsEmpty (Boolean -> a)
  -- | Increase a
  -- | GetVal (Int -> a)

data Message
  = New Block'
  | Updated Block'
  | Removed

myBlockViewer :: forall m. H.Component HH.HTML Query Unit Message m
myBlockViewer =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Just $ Block' addBlock [Just (intBlock' 1),Just (Block' addBlock [Just (intBlock' 1),Just (intBlock' 1)])]

  render :: State -> H.ComponentHTML Query
  render structure =
      HH.div_ $ structureRendering structure
  
  structureRendering :: Maybe Block' -> Array (H.ComponentHTML Query)
  structureRendering (Just block'@(Block' (Block name fn output inputs) connected)) =
    [ HH.table_ $ structureToTable [BlockView block']
    ]
  --   [ HH.p_ [ HH.text (name <> " " <> foldr (<>) "" (map (\input -> show input <> " -> ") inputs) <> show output) ]
  --   , HH.h2_ [ HH.text ("width = " <> show (getHeight $ Just block')) ]
  --   , HH.ol_ $ map (HH.li_ <<< structureRendering) connected
  --   ]
  structureRendering Nothing =
    [ HH.p_ [ HH.text ("Nothing") ]
    ]
  
  structureToTable :: Array (BlockView) -> Array (H.ComponentHTML Query)
  structureToTable [] = []
  structureToTable mElems =
    [ HH.tr_ $ map (structureToCell) mElems
    ] <> structureToTable (getNextRow mElems)
  
  structureToCell :: BlockView -> H.ComponentHTML Query
  structureToCell VoidBlock = HH.td [classes ["empty"]] [ ]
  structureToCell (InputBlock typ) = HH.td [classes ["input", toClass typ]] [ ]
  structureToCell (BlockView block'@(Block' (Block name fn out ins) connected)) =
    HH.td [HP.colSpan (getHeight $ Just block'), classes [toClass out]]
      [ HH.text (name <> " :: " <> foldr (<>) "" (map (\input -> show input <> " -> ") ins) <> show out)
      ]
  
  getConnected :: BlockView -> Array BlockView
  getConnected VoidBlock = [VoidBlock]
  getConnected (InputBlock _) = [VoidBlock]
  getConnected (BlockView (Block' _ [])) = [VoidBlock]
  getConnected (BlockView (Block' (Block _ _ _ ins) connected)) = map maybeToBlockView $ zip ins connected
  
  maybeToBlockView :: (Tuple Type (Maybe Block')) -> BlockView
  maybeToBlockView (Tuple typ Nothing) = InputBlock typ
  maybeToBlockView (Tuple typ (Just block')) = BlockView block'
  
  getNextRow :: Array BlockView -> Array BlockView
  getNextRow mElems
    | all (not <<< isBlockView) mElems = []
    | otherwise = concatMap getConnected mElems
  
  isBlockView :: BlockView -> Boolean
  isBlockView (BlockView _) = true
  isBlockView _ = false
  
  classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
  classes = HP.classes <<< map HCore.ClassName
  
  toClass :: Type -> String
  toClass IntType = "int"
  toClass ColourType = "colour"

  eval :: Query ~> H.ComponentDSL State Query Message m
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
    IsEmpty reply -> do
      state <- H.get
      pure (reply (isJust state))