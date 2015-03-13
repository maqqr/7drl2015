module Drawing where

import Data.Tuple
import Data.Char
import Data.Maybe
import Data.Foldable
import Data.String hiding (length, drop)
import Data.Array ((!!), (..), map, range, length, head, filter)
import Graphics.CanvasConsole
import qualified Data.Map as M
import Math

import GameState
import GameData
import Level
import Utils
import Line


-- Draws the message buffer.
drawMessages :: Console -> Point -> Number -> [String] -> ConsoleEff Unit
drawMessages console p col (x:xs) = drawString console x (rgb col col col) p.x p.y
                                 >> drawMessages console {x: p.x, y: p.y - 1} (col - 50) xs
drawMessages _       _ _   []     = return unit

-- Draws an array of strings.
drawStrings :: Console -> Point -> String -> [String] -> ConsoleEff Unit
drawStrings console p col (x:xs) = drawString console x col p.x p.y
                                >> drawStrings console {x: p.x, y: p.y + 1} col xs
drawStrings _ _  _  []           = return unit

-- Draws the game when state.blink changes to true or false.
blinkDraw :: Console -> GameState -> ConsoleEff GameState
blinkDraw console g@(Game state) | state.blinkTimer == 0 = drawGame console g
blinkDraw console g@(Game state) | otherwise             = return g

-- Draws the game state.
drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console g@(Game state@{ window = SkillW }) = do
    clear console
    drawString console ("Name: " ++ state.playerName ++ " Points: " ++ show state.points) "AAAAAA" 2 2
    drawString console (skillsInfo state.skills) "AAAAAA" 2 6
    drawString console (statsToString state.player.stats) "AAAAAA" 3 4
    drawString console "Press ESC or A to close skill window." "AAAAAA" 1 18
    return g
drawGame console g@(Game state@{ window = EquipW }) = do
    clear console
    drawString console "Press e to continue and i to open your inventory." "AAAAAA" 1 1
    drawString console "Items equipped: " "AAAAAA" 2 4
    drawString console (equipmentsToString state.equipments) "AAAAAA" 3 6
    drawString console "Press ESC or E to close equipment window." "AAAAAA" 1 18
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return g
drawGame console g@(Game state@{ window = InventoryW { index = page , command = com, equip = equ }, inventory = inv }) = do
    clear console
    drawString console "Press i to continue and e to open your equipments. Change page with + and -." "AAAAAA" 1 1
    drawString console ("Inventory (page " ++ show (page + 1) ++ "/" ++ show (floor ((length inv) / 10) + 1) ++ "): (Carrying: " ++ (show $ carryingWeight inv) ++ " lbs)") "AAAAAA" 2 4
    drawInventoryPage inv 0 (page * 10) 4 5
    drawString console "Press ESC or I to close inventory window, D to drop item." "AAAAAA" 1 18
    drawCommandLine com
    drawChoseEquipment equ
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return g
        where
            drawItemInfo :: Maybe Item -> Number -> Number -> ConsoleEff Unit
            drawItemInfo (Just i) x y = drawString console (showItem i) "AAAAAA" x y
            drawItemInfo _ x y        = drawString console "empty"      "AAAAAA" x y

            drawInventoryPage :: [Item] -> Number -> Number -> Number -> Number -> ConsoleEff Unit
            drawInventoryPage [] num i x y = drawString console "--- Empty ---" "AAAAAA" x y --End recursion when list is empty.
            drawInventoryPage _ 10 _ x y        = return unit
            drawInventoryPage items num i x y  = do
                drawItemInfo ((!!) items i) (x + 5) y
                drawString console ("(" ++ show num ++ "): ") "AAAAAA" x y
                drawInventoryPage items (num + 1) (i + 1) x (y + 1)

            drawCommandLine :: InventoryCommand -> ConsoleEff Unit
            drawCommandLine Drop = drawString console "Which item do you want to drop?" "AAAAAA" 1 19
            drawCommandLine Use  = drawString console "Which item do you want to drop?" "AAAAAA" 1 19
            drawCommandLine _    = return unit

            drawChoseEquipment :: Maybe EquipmentSlot -> ConsoleEff Unit
            drawChoseEquipment (Just slot) = drawString console ("Which item do you want to equip " ++ show slot ++ "?") "AAAAAA" 1 19
            drawChoseEquipment Nothing = return unit
drawGame console g@(Game state) = do
    clear console
    let offset = {x: 40 - state.player.pos.x, y: 10 - state.player.pos.y}
    mapM_ (tileDrawer offset) viewportPoints
    mapM_ (drawItem offset) state.items
    mapM_ (drawCreature offset) state.npcs
    drawCreature offset state.player
    drawString console ("HP: " ++ show (state.player.stats.hp) ++ "/" ++ show (state.player.stats.maxHp)) "FF0000" 2 24
    drawString console ("Points: " ++ (show (state.points))) "FF0000" 14 24
    drawString console ("Movement mode: " ++ show state.move ++ "   Speed: " ++ show (calcSpeed g)) "FF0000" 30 24
    drawString console (showPoint state.player.pos) "FF0000" 70 24
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return $ Game state { memory = updateMemory state.memory }
    where
        updateMemory :: Memory -> Memory
        updateMemory m' = foldl (\m p -> M.insert (Tuple p.x p.y) true m) m' $ filter playerCanSee (pointsAroundPlayer 12)

        viewportPoints :: [Point]
        viewportPoints = do
            x' <- 0 .. 79
            y' <- 0 .. 19
            return {x: x', y: y'}

        inViewport :: Point -> Boolean
        inViewport p = p.x >= 0 && p.y >= 0 && p.x < 80 && p.y < 20

        pointsAroundPlayer :: Number -> [Point]
        pointsAroundPlayer r = do
            x' <- (state.player.pos.x - r) .. (state.player.pos.x + r)
            y' <- (state.player.pos.y - r) .. (state.player.pos.y + r)
            return {x: x', y: y'}

        tileDrawer :: Point -> Point -> ConsoleEff Unit
        tileDrawer offset p = drawTileWithFov pInWorld (drawChar' p) (getTile state.level pInWorld)
            where
                pInWorld = p .-. offset

        drawChar' :: Point -> String -> String -> ConsoleEff Unit
        drawChar' pos c col = drawChar console c col pos.x pos.y

        drawCreature :: Point -> Creature -> ConsoleEff Unit
        drawCreature offset c | playerCanSee c.pos && inViewport (c.pos .+. offset) =
            drawAlertness (c.pos .+. offset .+. {x: 0, y: -1}) c.ai
            >> drawFacing (c.pos .+. offset) c.dir
            >> drawCreatureType (drawChar' (c.pos .+. offset)) c.ctype
        drawCreature offset c | otherwise = return unit

        drawAlertness :: Point -> AI -> ConsoleEff Unit
        drawAlertness p (AI _        Sleep)    | state.blink = drawChar' p "Z" "FFFFFF"
        drawAlertness p (AI MightSee _ )       | state.blink = drawChar' p "?" "555555"
        drawAlertness p (AI (Suspicious _) _ ) | state.blink = drawChar' p "?" "FFFF11"
        drawAlertness p (AI (Alert _) _ )      | state.blink = drawChar' p "!" "FF0000"
        drawAlertness p _ = return unit

        drawFacing :: Point -> Point -> ConsoleEff Unit
        drawFacing p dir | dir.x < 0 && state.blink = drawChar' (p .+. {x: -1, y: 0}) ">" "00FF00"
        drawFacing p dir | dir.x > 0 && state.blink = drawChar' (p .+. {x:  1, y: 0}) "<" "00FF00"
        drawFacing p dir | otherwise = return unit

        drawCreatureType :: forall a. (String -> String -> a) -> CreatureType -> a
        drawCreatureType d Player  = d (fromCode 240) "FF0000"
        drawCreatureType d Guard   = d (fromCode 240) "0000FF"
        drawCreatureType d Archer  = d (fromCode 240) "FF2EB9"
        drawCreatureType d Peasant = d (fromCode 240) "AAAAFF"
        drawCreatureType d _       = d "?" "FFFFFF"

        fromCode :: Number -> String
        fromCode = singleton <<< fromCharCode

        drawItem :: Point -> Item -> ConsoleEff Unit
        drawItem offset i | playerCanSee i.pos && inViewport (i.pos .+. offset) =
            drawItemType (drawChar' (i.pos .+. offset)) i.itemType
        drawItem offset i | otherwise = return unit

        drawItemType :: forall a. (String -> String -> a) -> ItemType -> a
        drawItemType d (Loot _)   = d "$" "FFAA00"
        drawItemType d (Weapon _) = d "/" "AAAAAA"
        drawItemType d (Armor _)  = d "[" "777777"
        drawItemType d (Shield _) = d "o" "999999"
        drawItemType d Ring       = d "," "DDDD00"
        drawItemType d _ = d "?" "FFFFFF"

        drawTileWithFov :: forall a. Point -> (String -> String -> a) -> Maybe Tile -> a
        drawTileWithFov p d t | playerCanSee p                        = drawTile false d t
        drawTileWithFov p d t | M.member (Tuple p.x p.y) state.memory = drawTile true d t
        drawTileWithFov p d t | otherwise                             = d (fromCode 178) "111111"

        playerCanSee :: Point -> Boolean
        -- playerCanSee p = true
        playerCanSee p | distanceSq state.player.pos p > 12 * 12 = false
        playerCanSee p | isTransparent state.level (state.player.pos .+. {x: 0, y: -1}) =
            lineOfSight state.level state.player.pos p
            || lineOfSight state.level (state.player.pos .+. {x: 0, y: -1}) p
        playerCanSee p | otherwise = lineOfSight state.level state.player.pos p

        drawTile :: forall a. Boolean -> (String -> String -> a) -> Maybe Tile -> a
        drawTile dr d (Just Air)        = d (fromCode 176) $ iff dr "222222" "002456"
        drawTile dr d (Just Ground)     = d (fromCode 176) $ iff dr "444444" "AAAAAA"
        drawTile dr d (Just Grass)      = d (fromCode 176) $ iff dr "444444" "009900"
        drawTile dr d (Just Wall)       = d (fromCode 219) $ iff dr "444444" "444422"
        drawTile dr d (Just SWall)      = d (fromCode 219) $ iff dr "444444" "444422"
        drawTile dr d (Just BrickWall)  = d (fromCode 220) $ iff dr "444444" "666666"
        drawTile dr d (Just DoorLocked) = d "+" $ iff dr "444444" "661111"
        drawTile dr d (Just DoorClosed) = d "+" $ iff dr "444444" "666633"
        drawTile dr d (Just DoorOpen)   = d "|" $ iff dr "444444" "666633"
        drawTile dr d (Just BgCave)     = d (fromCode 177) $ iff dr "333333" "666666"
        drawTile dr d (Just BgHouse)    = d (fromCode 219) $ iff dr "222222" "222205"
        drawTile dr d (Just Bush)       = d (fromCode 172) $ iff dr "444444" "009900"
        drawTile dr d (Just Water)      = d (fromCode 247) $ iff dr "444444" "0000FF"
        drawTile dr d (Just Stairs)     = d "<" $ iff dr "444444" "444422"
        drawTile dr d (Just Trunk)      = d "I" $ iff dr "444444" "666633"
        drawTile dr d (Just Leaves)     = d (fromCode 5) $ iff dr "444444" "005500"
        drawTile dr d _                 = d "?" $ iff dr "444444" "FFFFFF"

        -- Darker color for unseen, but remembered tiles.
        iff dr col1 col2 = if dr then col1 else col2

drawGame console MainMenu = do
    clear console
    drawString console " ______      _     _                    ______ _     " "FFFFFF" 5 5
    drawString console " | ___ \\    | |   | |                   | ___ \\ |    " "FFFFFF" 5 6
    drawString console " | |_/ /___ | |__ | |__   ___ _ __ _   _| |_/ / |    " "FFFFFF" 5 7
    drawString console " |    // _ \\| '_ \\| '_ \\ / _ \\ '__| | | |    /| |    " "FFFFFF" 5 8
    drawString console " | |\\ \\ (_) | |_) | |_) |  __/ |  | |_| | |\\ \\| |____" "FFFFFF" 5 9
    drawString console " \\_| \\_\\___/|_.__/|_.__/ \\___|_|   \\__, \\_| \\_\\_____/" "FFFFFF" 5 10
    drawString console "                                    __/ |            " "FFFFFF" 5 11
    drawString console "                                   |___/             " "FFFFFF" 5 12
    drawString console "Press enter to start your adventure" "AAAAAA" 6 16
    return MainMenu
drawGame console (NameCreation {playerName = pname}) = do
    clear console
    drawString console "What is your name? (max 15 characters):" "FF0000" 4 4
    drawString console pname "FF0000" 6 6
    drawString console "Press enter to continue" "FF0000" 4 8
    return (NameCreation {playerName: pname})
drawGame console (CharCreation {playerName = pname}) = do
    clear console
    drawString console ("Character Creation:  " ++ pname) "FF0000" 2 2

    drawString console "(SP = skill point)" "AAAAAA" 3 4

    drawString console "Press 0-8 to select character class" "FF0000" 2 20

    drawString console "(0) Archer      ( +2 dex | +1 str | +2 SP )" "336600" 3 6
    drawString console "(1) Knight      ( +4 str | -1 dex | +2 SP )" "B8B8B8" 3 7
    drawString console "(2) Monk        ( +2 int | +1 dex | +2 SP )" "E6AC00" 3 8
    drawString console "(3) Ninja       ( +4 dex | -1 str | +2 SP )" "5C5C5C" 3 9
    drawString console "(4) Peasant     ( -1 all |        | +1 SP )" "00B36B" 3 10
    drawString console "(5) Rogue       ( +1 dex | +1 int | +5 SP )" "5C5C5C" 3 11
    drawString console "(6) Scholar     ( +3 int | -1 str | +5 SP )" "3D00CC" 3 12
    drawString console "(7) Skillmaster ( +1 int |        | +8 SP )" "00B336" 3 13
    drawString console "(8) Soldier     ( +2 str | +1 dex | +2 SP )" "9E9E9E" 3 14

    --drawString console "Prefix:" "FF0000" 46 13---------------------- TODO (if time)
    --drawString console "j) Strong (+1 str)" "336600" 47 15
    --drawString console "k) Weak   (-1 str)" "FF0000" 47 16
    --drawString console "l) Agile  (+1 dex)" "336600" 47 17
    --drawString console "m) Clumsy (-1 dex)" "FF0000" 47 18
    --drawString console "n) Wise   (+1 int)" "336600" 47 19
    --drawString console "q) Dumb   (-1 int)" "FF0000" 47 20
    return (CharCreation {playerName: pname})
drawGame console (UseSkillPoints { playerName = pname, skillPoints = sp, skills = s, player = p }) = do
    clear console
    drawString console ("Name: " ++ pname) "FF0000" 2 2
    drawString console (statsToString p.stats) "FF0000" 2 3
    drawString console ((drawSkillsInfo 0) $ M.toList s) "FF0000" 3 5

    drawString console ("Skill points left: " ++ show sp) "FF0000" 2 18
    drawString console "Press 0-3 to select skill. Used skill points can't be taken back!" "FF0000" 2 20
    return (UseSkillPoints { playerName: pname, skillPoints: sp, skills: s, player: p })
        where
            drawSkillInfo :: Number -> Tuple SkillType Skill -> String
            drawSkillInfo i (Tuple sType { level = l, prog = p }) = fill ("(" ++ show i ++ "): " ++ show sType ++ " skill. ") 25 ++ " Level: " ++ show l ++ "."
            
            drawSkillsInfo :: Number -> [Tuple SkillType Skill] -> String
            drawSkillsInfo i [] = ""
            drawSkillsInfo i (x:xs) = drawSkillInfo i x ++ ("\n") ++ drawSkillsInfo (i + 1) xs

drawGame console (Death { playerName = pname, points = p }) = do
    clear console
    drawString console (pname ++ " has died.\n\nScore: " ++ show p ++ "\n\n\n\nPress Enter to start again.") "990000" 5 4
    return (Death { playerName: pname, points: p })
