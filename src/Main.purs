module Main where

import Data.Char
import Data.String
import Data.Maybe
import Data.Tuple
import Data.Array (map, updateAt)
import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace

import Graphics.CanvasConsole
import GameData
import Utils
import Level


type GameState = { level :: Level, player :: Creature, npcs :: [Creature] }

initialState :: GameState
initialState = { level: stringToLevel testLevel, player: pl, npcs: [testGuard] }
    where
        pl :: Creature
        pl = { pos: {x: 3, y: 3}, ctype: Player, stats: defaultStats }

        testGuard = { pos: {x: 10, y: 4}, ctype: Guard, stats: defaultStats }


onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt state = do
    drawGame console state
    return state

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console state = do
    clear console
    mapM_ (\p -> drawTile p (getTile state.level p)) (levelPoints state.level)
    mapM_ drawCreature state.npcs
    drawCreature state.player
    return state
    where
        drawCreature :: Creature -> ConsoleEff Unit
        drawCreature c = drawCreatureType c.pos c.ctype

        drawCreatureType :: Point -> CreatureType -> ConsoleEff Unit
        drawCreatureType p Player  = drawChar console "@" "FF0000" p.x p.y
        drawCreatureType p Guard   = drawChar console "G" "0000FF" p.x p.y
        drawCreatureType p Archer  = drawChar console "A" "00FF00" p.x p.y
        drawCreatureType p Peasant = drawChar console "P" "AAAAFF" p.x p.y
        drawCreatureType p _       = drawChar console "?" "FFFFFF" p.x p.y

        drawTile :: Point -> Maybe Tile -> ConsoleEff Unit
        drawTile p (Just Air)        = drawChar console "." "FFFFFF" p.x p.y
        drawTile p (Just Ground)     = drawChar console "#" "AAAAAA" p.x p.y
        drawTile p (Just Wall)       = drawChar console "#" "331A00" p.x p.y
        drawTile p (Just DoorLocked) = drawChar console "+" "331A00" p.x p.y
        drawTile p (Just DoorClosed) = drawChar console "+" "331A00" p.x p.y
        drawTile p (Just DoorOpen)   = drawChar console "|" "331A00" p.x p.y
        drawTile p _                 = drawChar console "?" "FFFFFF" p.x p.y


updateCreatures :: GameState -> GameState
updateCreatures state' = foldl updateCreature state' (enumerate state'.npcs)
    where
        -- updateCreature is not allowed to remove creatures.
        -- Dead creatures will be cleaned up later.
        updateCreature :: GameState -> Tuple Number Creature -> GameState
        updateCreature state (Tuple i c) = state { npcs = updateAt i (c { pos = {x: c.pos.x, y: c.pos.y + 1 }}) state.npcs }

updateWorld :: GameState -> GameState
updateWorld = updateCreatures

isValidMove :: Level -> Point -> Boolean
isValidMove level = isTileSolid <<< fromMaybe Air <<< getTile level

movePlayer :: Tuple Number Number -> GameState -> GameState
movePlayer (Tuple dx dy) state | (isValidMove (state.level) ({ x: state.player.pos.x + dx, y: state.player.pos.y + dy })) == true = state
movePlayer (Tuple dx dy) state | otherwise = updateWorld $ state { player = state.player { pos = clampPos { x: state.player.pos.x + dx, y: state.player.pos.y + dy } } }
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
