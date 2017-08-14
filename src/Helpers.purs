module Helpers where

import Prelude
import Structures
import Data.String as Str
import Data.Array (filter, foldr)
import Control.Monad.Eff (Eff)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Halogen.HTML.Core as HCore
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation, preventDefault)

classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
classes = HP.classes <<< map HCore.ClassName <<< filter (not <<< Str.null)

nil :: forall f . Applicative f => f Unit
nil = pure unit

data Message
  = SetDrag
  | StopDrag
  | HandleDrop
derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where show x = genericShow x

typeSignature :: Block -> String
typeSignature (Block name fn out ins) = " \x2237 " <> foldr (<>) "" (map (\input -> show input <> " \x2192 ") ins) <> show out

foreign import setEffectAllowed :: forall eff. String -> DragEvent -> Eff (dom :: DOM | eff) Unit
foreign import setDropEffect :: forall eff. String -> DragEvent -> Eff (dom :: DOM | eff) Unit
foreign import isInput :: forall eff. DragEvent -> Eff (dom :: DOM | eff) Boolean
foreign import inputValue :: forall eff. Event -> Eff (dom :: DOM | eff) String
foreign import setValue :: forall eff. Event -> String -> Eff (dom :: DOM | eff) Unit