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


data GameState = Game { level :: Level, player :: Creature, npcs :: [Creature] }
               | MainMenu
               | NameCreation { playerName :: String }
               | CharCreation { playerName :: String }

initialState :: GameState
initialState = Game { level: stringToLevel testLevel, player: pl, npcs: [testGuard] }
    where
        pl :: Creature
        pl = { pos: {x: 3, y: 3}, ctype: Player, stats: defaultStats }

        testGuard = { pos: {x: 10, y: 4}, ctype: Guard, stats: defaultStats }


onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt st = do
    drawGame console st
    return st

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console st@(Game state) = do
    clear console
    mapM_ (\p -> drawTile p (getTile state.level p)) (levelPoints state.level)
    mapM_ drawCreature state.npcs
    drawCreature state.player
    return st
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
drawGame console MainMenu = do
    clear console
    drawString console "RobberyRL" "FFFFFF" 12 5
    drawString console "Press enter to start your adventure" "AAAAAA" 6 12
    return MainMenu
drawGame console (NameCreation {playerName = pname}) = do
    clear console
    drawString console "What is your name? (max 15 characters):" "FF0000" 4 4
    drawString console pname "FF0000" 6 6
    drawString console "Press enter to continue" "FF0000" 4 8
    return (NameCreation {playerName: pname})
drawGame console (CharCreation {playerName = pname}) = do
    clear console
    drawString console ("Character Creation:   " ++ pname) "FF0000" 2 2

    drawString console "This is a testline, press a to enter game" "FF0000" 2 20

    drawString console "a) Archer      ( +2 dex | +1 str | +2 SP ) (SP = skill point)" "336600" 3 4
    drawString console "b) Knight      ( +4 str | -1 dex | +2 SP )" "B8B8B8" 3 5
    drawString console "c) Monk        ( +2 int | +1 dex | +2 SP )" "E6AC00" 3 6
    drawString console "d) Ninja       ( +4 dex | -1 str | +2 SP )" "5C5C5C" 3 7
    drawString console "e) Peasant     ( -1 all |        | +1 SP )" "00B36B" 3 8
    drawString console "f) Rogue       ( +1 dex | +1 int | +5 SP )" "5C5C5C" 3 9
    drawString console "g) Scholar     ( +3 int | -1 str | +5 SP )" "3D00CC" 3 10
    drawString console "h) Skillmaster ( +1 int |        | +8 SP )" "00B336" 3 11
    drawString console "i) Soldier     ( +2 str | +1 dex | +2 SP )" "9E9E9E" 3 12

    --drawString console "Prefix:" "FF0000" 46 13
    --drawString console "j) Strong (+1 str)" "336600" 47 15
    --drawString console "k) Weak   (-1 str)" "FF0000" 47 16
    --drawString console "l) Agile  (+1 dex)" "336600" 47 17
    --drawString console "m) Clumsy (-1 dex)" "FF0000" 47 18
    --drawString console "n) Wise   (+1 int)" "336600" 47 19
    --drawString console "q) Dumb   (-1 int)" "FF0000" 47 20
    return (CharCreation {playerName: pname})


updateCreatures :: GameState -> GameState
updateCreatures st@(Game state') = foldl updateCreature st (enumerate state'.npcs)
    where
        -- updateCreature is not allowed to remove creatures.
        -- Dead creatures will be cleaned up later.
        updateCreature :: GameState -> Tuple Number Creature -> GameState
        updateCreature (Game state) (Tuple i c) = Game $ state { npcs = updateAt i (c { pos = {x: c.pos.x, y: c.pos.y + 1 }}) state.npcs }

updateWorld :: GameState -> GameState
updateWorld = updateCreatures

isValidMove :: Level -> Point -> Boolean
isValidMove level = isTileSolid <<< fromMaybe Air <<< getTile level

movePlayer :: Tuple Number Number -> GameState -> GameState
movePlayer (Tuple dx dy) (Game state) | (isValidMove (state.level) ({ x: state.player.pos.x + dx, y: state.player.pos.y + dy })) == true = Game state
movePlayer (Tuple dx dy) (Game state) | otherwise = updateWorld $ Game $ state { player = state.player { pos = clampPos { x: state.player.pos.x + dx, y: state.player.pos.y + dy } } }
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
onKeyPress console st@(Game state) key =
    case M.lookup key movementkeys of
        Just delta -> drawGame console $ movePlayer delta st
        Nothing    -> return st
onKeyPress console MainMenu key                            | key == 13      = return $ NameCreation { playerName: "" }
onKeyPress console (NameCreation {playerName = pname}) key | key == 13      = return $ CharCreation { playerName: pname }
onKeyPress console (NameCreation {playerName = xs}) key    | key == 8       = return $ NameCreation { playerName: (take (length xs - 1) xs) }
onKeyPress console (NameCreation {playerName = ""}) key                     = return $ NameCreation { playerName: (fromCharArray [fromCharCode key]) }
onKeyPress console (NameCreation {playerName = xs}) key    | length xs > 15 = return $ NameCreation { playerName: xs }
onKeyPress console (NameCreation {playerName = xs}) key                     = return $ NameCreation { playerName: (xs ++ (fromCharArray [fromCharCode key])) }
onKeyPress console (CharCreation pname) key                | key == 65      = return initialState --lisää pelaajan nimen vienti initialStateen
onKeyPress _ st _ = return st


main = J.ready $ withConsole 80 25 MainMenu {onKeyPress: onKeyPress, onUpdate: onUpdate}
