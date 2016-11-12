module Main where

import Prelude (bind, Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Pux (start, fromSimple, renderToDOM, CoreEffects)
import Game (init, update, view)

main :: forall e. Eff (console :: CONSOLE | CoreEffects e) Unit
main = do
  log "What the fuck?"
  app <- start 
    { initialState: init 
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
