module Main where

import Data.Char
import Data.String hiding (length)
import Data.Maybe
import Data.Tuple
import Data.Array ((!!), (..), map, updateAt, modifyAt, range, length, head, filter)
import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace

import Graphics.CanvasConsole
import GameData
import Utils
import Level

strlen = Data.String.length

data GameState = Game { level         :: Level
                      , player        :: Creature
                      , npcs          :: [Creature]
                      , items         :: [Item]
                      , playerName    :: String
                      , points        :: Number
                      , inventory     :: [Item]
                      , freeFallTimer :: Number
                      }
               | MainMenu
               | NameCreation { playerName :: String }
               | CharCreation { playerName :: String }

initialState :: String -> GameState
initialState pname = Game
        { level: stringToLevel testLevel
        , player: pl
        , npcs: [testGuard]
        , items: [testItem1, testItem2]
        , playerName: pname
        , points: 0
        , inventory: []
        , freeFallTimer: 0
        }
    where
        pl :: Creature
        pl = { pos: {x: 4, y: 3}, ctype: Player, stats: defaultStats, speed: 1000, time: 0, vel: zero }

        testGuard = { pos: {x: 10, y: 4}, ctype: Guard, stats: defaultStats, speed: 500, time: 0, vel: zero }
        testItem1 = { itemType: Weapon { dmg: 1, attackBonus: 1 }, pos: {x: 6, y: 4}, vel: {x: 0, y: 0}, weight: 4 }
        testItem2 = { itemType: Loot { value: 3 }, pos: {x: 6, y: 3}, vel: {x: 0, y: 0}, weight: 1 }


onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt g@(Game state) | inFreeFall state.level state.player && state.freeFallTimer > 0.1 =
    drawGame console <<< updateWorld (calcSpeed g state.player) $ Game state { freeFallTimer = 0 }
onUpdate console dt g@(Game state) | otherwise =
    drawGame console $ Game state { freeFallTimer = state.freeFallTimer + dt }
onUpdate console _ g = drawGame console g

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console g@(Game state) = do
    clear console
    mapM_ (\p -> drawTile p (getTile state.level p)) (levelPoints state.level)
    mapM_ drawCreature state.npcs
    mapM_ drawItem state.items
    drawCreature state.player
    drawString console (state.playerName) "FF0000" 2 23
    drawString console ("HP: " ++ (show (state.player.stats.hp))) "FF0000" 2 24
    drawString console ("Points: " ++ (show (state.points))) "FF0000" 10 24
    return g
    where
        drawCreature :: Creature -> ConsoleEff Unit
        drawCreature c = drawCreatureType c.pos c.ctype

        drawCreatureType :: Point -> CreatureType -> ConsoleEff Unit
        drawCreatureType p Player  = drawChar console "@" "FF0000" p.x p.y
        drawCreatureType p Guard   = drawChar console "G" "0000FF" p.x p.y
        drawCreatureType p Archer  = drawChar console "A" "00FF00" p.x p.y
        drawCreatureType p Peasant = drawChar console "P" "AAAAFF" p.x p.y
        drawCreatureType p _       = drawChar console "?" "FFFFFF" p.x p.y

        drawItem :: Item -> ConsoleEff Unit
        drawItem i = drawItemType i.pos i.itemType

        drawItemType :: Point -> ItemType -> ConsoleEff Unit
        drawItemType p (Loot _)   = drawChar console "$" "FFAA00" p.x p.y
        drawItemType p (Weapon _) = drawChar console "/" "AAAAAA" p.x p.y

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

    drawString console "Press a to enter game" "FF0000" 2 20

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


-- Updates all npcs.
updateCreatures :: Number -> GameState -> GameState
updateCreatures advance g@(Game state') =
    foldl (\g' i -> updateCreature i (advTime i g')) g (0 .. (length state'.npcs - 1))
    where
        -- Updaters are not allowed to remove creatures.
        -- Dead creatures will be cleaned up later.

        -- Advances game time for creature at index i.
        advTime :: Number -> GameState -> GameState
        advTime i game = modifyCreatureAt i game $ \c -> c { time = c.time + advance }

        -- Gets creature information from creature at index i.
        get :: forall a. Number -> GameState -> a -> (Creature -> a) -> a
        get i (Game state) d getter = fromMaybe d $ getter <$> state.npcs !! i

        -- Modify creature at index i with a function.
        modifyCreatureAt :: Number -> GameState -> (Creature -> Creature) -> GameState
        modifyCreatureAt i (Game state) f = Game state { npcs = modifyAt i f state.npcs }

        -- Updates creature at index i until it runs out of time.
        updateCreature :: Number -> GameState -> GameState
        updateCreature i game | get i game 0 (\c -> c.time) >= get i game 0 (calcSpeed game) =
            updateCreature i (modifyCreatureAt i (updateCreatureOnce i game) (\c -> c { time = c.time - calcSpeed game c }))
        updateCreature i game | otherwise = game

        -- Creature at index i does a single action.
        updateCreatureOnce :: Number -> GameState -> GameState
        updateCreatureOnce i game = modifyCreatureAt i game $ updatePhysics game

updatePhysics :: forall r. GameState -> { pos :: Point, vel :: Point | r } -> { pos :: Point, vel :: Point | r }
updatePhysics g@(Game state) c | inFreeFall state.level c = move g fc (unitp fc.vel)
    where
        projectedMovement p = p.pos .+. unitp p.vel

        -- 1. update gravity
        gc = c { vel = { x: c.vel.x, y: c.vel.y + 1 } }

        -- 2. check collision
        newpos = projectedMovement gc
        blocked = not $ isValidMove state.level newpos
        finalVelX = if blocked then 0 else gc.vel.x

        -- 3. calculate final velocity
        fc = c { vel = { x: finalVelX, y: gc.vel.y } }


updatePhysics _ c = c { vel = zero}

move :: forall r. GameState -> { pos :: Point | r } -> Point -> { pos :: Point | r }
move (Game state) c delta =
    if blocked then c else c { pos = newpos }
    where
        newpos  = clampPos $ c.pos .+. delta
        blocked = not $ isValidMove state.level newpos

        clampPos :: Point -> Point
        clampPos pos = { x: clamp pos.x 0 79, y: clamp pos.y 0 24 }

updateWorld :: Number -> GameState -> GameState
updateWorld advance = updateCreatures advance
    >>> (\(Game state) -> Game state { player = updatePhysics (Game state) state.player })
    >>> (\(Game state) -> Game state { items = map (updatePhysics (Game state)) state.items })

inFreeFall :: forall r. Level -> { pos :: Point | r } -> Boolean
inFreeFall level c = isValidMove level (c.pos .+. {x:0, y: 1})

isValidMove :: Level -> Point -> Boolean
isValidMove level = not <<< isTileSolid <<< fromMaybe Air <<< getTile level

-- todo: item weight should affect player
calcSpeed :: GameState -> Creature -> Number
calcSpeed (Game state) c | inFreeFall state.level c = 500
calcSpeed (Game state) c | otherwise                = c.speed

movePlayer :: Point -> GameState -> GameState
movePlayer delta g@(Game state) =
    if canMove then
        updateWorld (calcSpeed g state.player) $ Game state { player = move (Game state) state.player delta }
        else Game state
    where
        newpos  = state.player.pos .+. delta
        canMove = isValidMove state.level newpos

jump :: GameState -> Number -> Creature -> Creature
jump g xdir c = move g (c { vel = {x: xdir, y: -2} }) {x: xdir, y: -1}

movementkeys :: M.Map Number Point
movementkeys = M.fromList [numpad 8 // {x:  0, y: -1}
                          ,numpad 2 // {x:  0, y:  1}
                          ,numpad 4 // {x: -1, y:  0}
                          ,numpad 6 // {x:  1, y:  0}
                          ,numpad 5 // {x:  0, y:  0}]

pickUp :: Point -> GameState -> GameState
pickUp point (Game state) =
    case head (filter (\i -> i.pos .==. point) state.items) of
        Just item -> Game state { items = deleteItem state.items point, inventory = addItem state.inventory item }
        Nothing   -> Game state
    where
        deleteItem :: [Item] -> Point -> [Item]
        deleteItem (x:xs) delPos | x.pos .==. delPos = xs
        deleteItem (x:xs) delPos | otherwise = x : deleteItem xs delPos
        deleteItem [] _ = []

        addItem :: [Item] -> Item -> [Item]
        addItem xs { itemType = Loot { value = num } } = xs
        addItem xs x = x:xs

onKeyPress :: Console -> GameState -> Number -> ConsoleEff GameState
onKeyPress console g@(Game state) _ | inFreeFall state.level state.player = return g
onKeyPress console g@(Game state) key                      | key == numpad 7 = return $ Game state { player = jump g (-1) state.player }
onKeyPress console g@(Game state) key                      | key == numpad 9 = return $ Game state { player = jump g 1 state.player }
onKeyPress console g@(Game state) key                      | key == 80       = return g
onKeyPress console g@(Game state) key =
    case M.lookup key movementkeys of
        Just delta -> drawGame console $ movePlayer delta g
        Nothing    -> return g

onKeyPress console MainMenu key                            | key == 13       = return $ NameCreation { playerName: "" }
onKeyPress console (NameCreation {playerName = pname}) key | key == 13       = return $ CharCreation { playerName: pname }
onKeyPress console (NameCreation {playerName = xs}) key    | key == 8        = return $ NameCreation { playerName: (take (strlen xs - 1) xs) }
onKeyPress console (NameCreation {playerName = ""}) key                      = return $ NameCreation { playerName: (fromCharArray [fromCharCode key]) }
onKeyPress console (NameCreation {playerName = xs}) key    | strlen xs > 15  = return $ NameCreation { playerName: xs }
onKeyPress console (NameCreation {playerName = xs}) key                      = return $ NameCreation { playerName: (xs ++ (fromCharArray [fromCharCode key])) }
onKeyPress console (CharCreation {playerName = xs}) key    | key == 65       = return $ initialState xs
onKeyPress _ st _ = return st


main = J.ready $ withConsole 80 25 MainMenu {onKeyPress: onKeyPress, onUpdate: onUpdate}
