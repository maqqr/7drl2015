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
import Utils

type GameState = { x :: Number, y :: Number }


drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console state = do
    clear console
    drawString console "Hello worlllddd" "0000FF" 2 8
    drawChar console "@" "FF0000" state.x state.y
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

numpad :: Number -> Number
numpad key = 96 + key


movementkeys :: M.Map Number (Tuple Number Number)
movementkeys = M.fromList [numpad 8 // ( 0 // -1)
                          ,numpad 2 // ( 0 //  1)
                          ,numpad 4 // (-1 //  0)
                          ,numpad 6 // ( 1 //  0)]

onKeyPress :: Console -> GameState -> Number -> ConsoleEff GameState
onKeyPress console state key = case M.lookup key movementkeys of
    Just delta -> drawGame console $ movePlayer delta state
    Nothing    -> return state

onKeyPress console state key = trace $ show key >> return state

main = J.ready $ withConsole 80 25 {x:3, y:3} {onKeyPress: onKeyPress}

