module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
--import Form as Form

import Form as Form


main :: Effect Unit
main = HA.runHalogenAff $
       HA.awaitBody >>= runUI Form.page unit




{-main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Form.component unit body
-}