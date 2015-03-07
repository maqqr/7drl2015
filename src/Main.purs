module Main where

import Data.Char
import Data.String
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace

import Graphics.CanvasConsole
import GameData
import Utils
import Level

type GameState = { level :: Level, playerPos :: Point }

initialState :: GameState
initialState = { level: stringToLevel testLevel, playerPos: {x:3, y:3} }


onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt state = do
    drawGame console state
    return state

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console state = do
    clear console
    mapM_ (\p -> drawTile p (getTile state.level p)) (levelPoints state.level)
    drawString console "Hello worlllddd" "0000FF" 2 8
    drawString console "This is a test line" "CCCC00" 5 21
    drawChar console "@" "FF0000" state.playerPos.x state.playerPos.y
    return state
    where
        drawTile :: Point -> Maybe Tile -> ConsoleEff Unit
        drawTile p (Just Air)    = drawChar console "." "FFFFFF" p.x p.y
        drawTile p (Just Ground) = drawChar console "#" "AAAAAA" p.x p.y
        drawTile p (Just Wall)   = drawChar console "#" "331A00" p.x p.y
        drawTile p _             = drawChar console "?" "FFFFFF" p.x p.y


movePlayer :: Tuple Number Number -> GameState -> GameState
movePlayer (Tuple dx dy) state = state { playerPos = clampPos { x: state.playerPos.x + dx, y: state.playerPos.y + dy } }
    where
        clamp x min max | x < min = min
        clamp x min max | x > max = max
        clamp x min max = x

        clampPos :: Point -> Point
        clampPos pos = { x: clamp pos.x 0 79, y: clamp pos.y 0 24 }


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

main = J.ready $ withConsole 80 25 initialState {onKeyPress: onKeyPress, onUpdate: onUpdate}
