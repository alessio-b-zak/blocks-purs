module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import DOM.HTML.Types (PROMPT)
import Blocks as Blocks

main :: Eff (HA.HalogenEffects (prompt :: PROMPT)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Blocks.blocksApp unit body