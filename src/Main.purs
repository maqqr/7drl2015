module Main where

import Data.Char
import Data.String
import Data.Maybe
import qualified Data.String.Unsafe as SU
import Data.Tuple
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Graphics.CanvasConsole
import Debug.Trace

type GameState = { x :: Number, y :: Number }

type ConsoleEff a = forall eff. Eff (con :: ConEff, trace :: Debug.Trace.Trace | eff) a

(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \_ -> g

(//) = Tuple

onInit :: Console -> GameState -> ConsoleEff Unit
onInit console state = do
    drawGame console state
    return unit

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console state = do
    clear console
    drawString console "Hello worlllddd" "blue" 2 8
    drawChar console "@" "red" state.x state.y
    return state

movePlayer :: Tuple Number Number -> GameState -> GameState
movePlayer (Tuple dx dy) state = clampPos $ state { x = state.x + dx, y = state.y + dy }
    where
        clamp x min max | x < min = min
        clamp x min max | x > max = max
        clamp x min max = x

        clampPos state = state { x = clamp state.x 0 79, y = clamp state.y 0 24 }


makeChar :: String -> Char
makeChar = SU.charAt 0

movementkeys :: M.Map Char (Tuple Number Number)
movementkeys = M.fromList [makeChar "W" // ( 0 // -1)
                          ,makeChar "S" // ( 0 //  1)
                          ,makeChar "A" // (-1 //  0)
                          ,makeChar "D" // ( 1 //  0)]

onKeyPress :: Console -> GameState -> Number -> ConsoleEff GameState
onKeyPress console state key = case M.lookup (fromCharCode key) movementkeys of
    Just delta -> drawGame console $ movePlayer delta state
    Nothing    -> return state

onKeyPress console state key = trace $ show key >> return state

main = J.ready $ withConsole 80 25 {x:3, y:3} onInit onKeyPress
