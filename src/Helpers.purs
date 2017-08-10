module Helpers where

import Prelude
import Data.String as Str
import Data.Array (filter)
import Halogen.HTML.Core as HCore
import Halogen.HTML.Properties as HP

classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
classes = HP.classes <<< map HCore.ClassName <<< filter (not <<< Str.null)

data Message
  = SetDrag
  | StopDrag
  | FetchDrag
  | DeleteDrag