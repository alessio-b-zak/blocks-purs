module Blocks where

import Prelude
import Structures
import Helpers
import BlockPicker as BP
import BlockViewer as BV

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_, for_)
import Data.Array ((!!))
import Data.Functor (void)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Either.Nested (Either3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.Aff (Aff)
import Debug.Trace

import DOM (DOM)
import DOM.HTML.Types (PROMPT)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (<\/>), type (\/))

type State = DragSource

type DragSource = Maybe Child

data Child = Picker | Viewer
derive instance eqChild :: Eq Child
derive instance genericChild :: Generic Child _
instance showChild :: Show Child where show x = genericShow x

data Query a = Query Child Message a

data GenericQuery = RemoveDragged | DropBlock

type ChildQuery = Coproduct3 BP.Query BV.Query BV.Query

type ChildSlot = Either3 Unit Unit Unit

blocksApp :: forall eff. H.Component HH.HTML Query Unit Void (Aff (prompt :: PROMPT, dom :: DOM | eff))
blocksApp =
  H.parentComponent
    { initialState: const Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (prompt :: PROMPT, dom :: DOM | eff))
  render state =
    HH.div [classes ["app"]]
      [ HH.slot' CP.cp1 unit BP.myBlockPicker unit (HE.input $ Query Picker)
      , HH.slot' CP.cp2 unit BV.myBlockViewer unit (HE.input $ Query Viewer)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (prompt :: PROMPT, dom :: DOM | eff))
  eval = case _ of
    Query source msg reply -> do
      case msg of
        SetDrag -> H.put (Just source)
        StopDrag -> do
          drag <- H.get
          when (drag == Just source) $ H.put Nothing
        HandleDrop -> do
          mBlock <- getDragged
          traceShowA mBlock
          for_ mBlock $ \block -> do
            traceShowA block
            maybeSuccess <- case source of
              Picker -> H.query' CP.cp1 unit (H.request (BP.DropBlock block))
              Viewer -> H.query' CP.cp2 unit (H.request (BV.DropBlock block))
            traceShowA maybeSuccess
            when (fromMaybe false maybeSuccess) removeDragged
      pure reply

  getDragged :: H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (prompt :: PROMPT, dom :: DOM | eff)) (Maybe Block')
  getDragged = do
    mDragSource <- H.get
    traceShowA mDragSource
    case mDragSource of
      Nothing -> pure Nothing
      Just Picker -> join <$> H.query' CP.cp1 unit (H.request BP.GetDragged)
      Just Viewer -> join <$> H.query' CP.cp2 unit (H.request BV.GetDragged)

  removeDragged :: H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (prompt :: PROMPT, dom :: DOM | eff)) Unit
  removeDragged = do
    mDragSource <- H.get
    case mDragSource of
      Just Viewer -> void $ H.query' CP.cp2 unit (H.request BV.RemoveDragged)
      _ -> nil
    H.put $ Nothing
      
      
    