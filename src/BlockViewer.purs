module BlockViewer where

import Prelude
import Structures
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr, all)
import Data.Array (concatMap, zip, filter, null)
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HCore

type State = Maybe Block'

data BlockView = BlockView Block' | InputBlock Type

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
  initialState = Just $ Block' addBlock [Just (intBlock' 1),Just (Block' addBlock [Just (intBlock' 1),{-Just (intBlock' 1)-}Nothing])]

  render :: State -> H.ComponentHTML Query
  render structure =
      HH.div_ $ structureRendering structure
  
  structureRendering :: Maybe Block' -> Array (H.ComponentHTML Query)
  structureRendering (Just block'@(Block' (Block name fn output inputs) connected)) =
    [ HH.div [classes ["structureView"]] [ structureToDiv (BlockView block') ]
    ]
  structureRendering Nothing =
    [ HH.p_ [ HH.text ("Nothing") ]
    ]
  
  structureToDiv :: BlockView -> H.ComponentHTML Query
  structureToDiv (InputBlock typ) = HH.div [classes ["input", toClass typ]] []
  structureToDiv blockView@(BlockView (Block' (Block name fn out ins) connected)) =
    HH.div [ HP.draggable true, classes ["block", toClass out, "void" `if_` null connected] ]
      (
        [ HH.div [classes ["block-text"]]
          [ HH.text (name <> " \x2237 " <> foldr (<>) "" (map (\input -> show input <> " \x2192 ") ins) <> show out) ]
        ]
        <> [ HH.div [classes ["block-inputs"]] $ map structureToDiv (getConnected blockView)] `if_` not (null connected)
      )
  
  getConnected :: BlockView -> Array BlockView
  getConnected (InputBlock _) = []
  getConnected (BlockView (Block' _ [])) = []
  getConnected (BlockView (Block' (Block _ _ _ ins) connected)) = map maybeToBlockView $ zip ins connected
  
  maybeToBlockView :: (Tuple Type (Maybe Block')) -> BlockView
  maybeToBlockView (Tuple typ Nothing) = InputBlock typ
  maybeToBlockView (Tuple typ (Just block')) = BlockView block'
  
  classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
  classes = HP.classes <<< map HCore.ClassName <<< filter (not <<< Str.null)
  
  toClass :: Type -> String
  toClass IntType = "int"
  toClass ColourType = "colour"
  
  if_ :: forall a . Monoid a => a -> Boolean -> a
  if_ x bool = if bool then x else mempty

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