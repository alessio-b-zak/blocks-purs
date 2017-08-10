module Blocks where

import Prelude
import Structures
import Helpers
import BlockPicker as BP
import BlockViewer as BV

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Array ((!!))
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Either.Nested (Either3)
import Control.Monad.Aff (Aff)

import DOM (DOM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP

type State = DragSource

type DragSource = Maybe Section

data Section = Picker | Viewer
derive instance eqSection :: Eq Section

data Query a = Query Section Message a

type ChildQuery = Coproduct3 BP.Query BV.Query BV.Query

type ChildSlot = Either3 Unit Unit Unit

blocksApp :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
blocksApp =
  H.parentComponent
    { initialState: const Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (dom :: DOM | eff))
  render state =
    HH.div [classes ["app"]]
      [ HH.slot' CP.cp1 unit BP.myBlockPicker unit (HE.input $ Query Picker)
      , HH.slot' CP.cp2 unit BV.myBlockViewer unit (HE.input $ Query Viewer)
      , HH.text $ if state == Nothing then "Nothing" else "Something"
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (dom :: DOM | eff))
  eval = case _ of
    Query source msg reply -> do
      case msg of
        SetDrag -> H.put (Just source)
        StopDrag -> do
          drag <- H.get
          when (drag == Just source) $ H.put Nothing
        DeleteDrag -> pure unit
        FetchDrag -> pure unit
      pure reply