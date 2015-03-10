module Main where

import Data.Char
import Data.String hiding (length, drop)
import Data.Maybe
import Data.Tuple
import Data.Array ((!!), (..), map, updateAt, modifyAt, range, length, head, filter, drop)
import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace

import Graphics.CanvasConsole
import GameData
import Utils
import Level
import Astar
import Line

strlen = Data.String.length

data GameWindow = GameW
                | EquipW
                | InventoryW

data GameState = Game { level         :: Level
                      , player        :: Creature
                      , npcs          :: [Creature]
                      , items         :: [Item]
                      , playerName    :: String
                      , points        :: Number
                      , skills        :: [Skill]
                      , inventory     :: [Item]
                      , freeFallTimer :: Number
                      , messageBuf    :: [String]
                      , pathfinder    :: Pathfinder
                      , window        :: GameWindow
                      }
               | MainMenu
               | NameCreation { playerName :: String }
               | CharCreation { playerName :: String }

initialState :: String -> GameState
initialState pname = Game
        { level: lvl
        , player: pl
        , npcs: [testGuard]
        , items: [testItem1, testItem2]
        , playerName: pname
        , points: 0
        , skills: defaultSkills
        , inventory: []
        , freeFallTimer: 0
        , messageBuf: map ((++) "Line" <<< show) (1 .. 4)
        , pathfinder: makePathfinder (levelWeights lvl)
        , window: GameW
        }
    where
        lvl = stringToLevel testLevel

        pl = { pos: {x: 4, y: 3}, ctype: Player, stats: defaultStats, time: 0, vel: zerop }

        testGuard = { pos: {x: 10, y: 20}, ctype: Guard, stats: defaultStats, time: 0, vel: zerop }
        testItem1 = { itemType: Weapon { dmg: 1, attackBonus: 1 }, pos: {x: 6, y: 4}, vel: {x: 0, y: 0}, weight: 4 }
        testItem2 = { itemType: Loot { value: 3 }, pos: {x: 20, y: 3}, vel: {x: 0, y: 0}, weight: 1 }

messageBufSize :: Number
messageBufSize = 4

addMsg :: String -> GameState -> GameState
addMsg msg (Game state) | length state.messageBuf >= messageBufSize =
    Game state { messageBuf = drop 1 state.messageBuf ++ [msg] }
addMsg msg (Game state) | otherwise =Game state { messageBuf = state.messageBuf ++ [msg] }

onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt g@(Game state) | playerCannotAct state.level state.player && state.freeFallTimer > 0.1 =
    drawGame console <<< updateWorld true (calcSpeed g state.inventory state.player) $ Game state { freeFallTimer = 0 }
onUpdate console dt g@(Game state) | otherwise =
    return $ Game state { freeFallTimer = state.freeFallTimer + dt }
onUpdate console _ g = drawGame console g

drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console g@(Game { window = EquipW }) = do
    --TODO: drawing equipment window
    return g
drawGame console g@(Game { window = InventoryW, inventory = inv }) = do
    drawInventory inv 4 4
    return g
        where
            drawItemInfo :: Item -> Number -> Number -> ConsoleEff Unit
            drawItemInfo i x y = drawString console (showItem i) "AAAAAA" x y
            drawItemInfo _ x y = drawString console "empty"      "AAAAAA" x y

            drawInventory :: [Item] -> Number -> Number -> ConsoleEff Unit
            drawInventory [] x y = drawString console "---" "AAAAAA" x y
            drawInventory (i:is) x y = do
                drawItemInfo i x y
                drawInventory is x (y + 1)
drawGame console g@(Game state) = do
    clear console
    let offset = {x: 40 - state.player.pos.x, y: 10 - state.player.pos.y}
    mapM_ (tileDrawer offset) viewportPoints
    mapM_ (drawItem offset) state.items
    mapM_ (drawCreature offset) state.npcs
    drawCreature offset state.player
    drawString console (state.playerName) "FF0000" 2 23
    drawString console ("HP: " ++ (show (state.player.stats.hp))) "FF0000" 2 24
    drawString console ("Points: " ++ (show (state.points))) "FF0000" 10 24
    drawStrings {x: 1, y: 20} "FFFFFF" state.messageBuf
    return g
    where
        viewportPoints :: [Point]
        viewportPoints = do
            x' <- 0 .. 79
            y' <- 0 .. 19
            return {x: x', y: y'}

        pointsAroundPlayer :: Number -> [Point]
        pointsAroundPlayer r = do
            x' <- (state.player.pos.x - r) .. (state.player.pos.x + r)
            y' <- (state.player.pos.y - r) .. (state.player.pos.y + r)
            return {x: x', y: y'}

        tileDrawer :: Point -> Point -> ConsoleEff Unit
        tileDrawer offset p = drawTileWithFov pInWorld (drawChar' p) (getTile state.level pInWorld)
            where
                pInWorld = p .-. offset

        drawStrings :: Point -> String -> [String] -> ConsoleEff Unit
        drawStrings p col (x:xs) = drawString console x col p.x p.y
                                >> drawStrings {x: p.x, y: p.y + 1} col xs
        drawStrings _  _  []     = return unit

        drawChar' :: Point -> String -> String -> ConsoleEff Unit
        drawChar' pos c col = drawChar console c col pos.x pos.y

        drawCreature :: Point -> Creature -> ConsoleEff Unit
        drawCreature offset c | playerCanSee c.pos = drawCreatureType (drawChar' (c.pos .+. offset)) c.ctype
        drawCreature offset c | otherwise          = return unit

        drawCreatureType :: forall a. (String -> String -> a) -> CreatureType -> a
        drawCreatureType d Player  = d "@" "FF0000"
        drawCreatureType d Guard   = d "G" "0000FF"
        drawCreatureType d Archer  = d "A" "00FF00"
        drawCreatureType d Peasant = d "P" "AAAAFF"
        drawCreatureType d _       = d "?" "FFFFFF"

        fromCode :: Number -> String
        fromCode = singleton <<< fromCharCode

        drawItem :: Point -> Item -> ConsoleEff Unit
        drawItem offset i | playerCanSee i.pos = drawItemType (drawChar' (i.pos .+. offset)) i.itemType
        drawItem offset i | otherwise          = return unit

        drawItemType :: forall a. (String -> String -> a) -> ItemType -> a
        drawItemType d (Loot _)   = d "$" "FFAA00"
        drawItemType d (Weapon _) = d "/" "AAAAAA"

        drawTileWithFov :: forall a. Point -> (String -> String -> a) -> Maybe Tile -> a
        drawTileWithFov p d t | playerCanSee p = drawTile d t
        drawTileWithFov p d t | otherwise      = d (fromCode 178) "111111"

        playerCanSee :: Point -> Boolean
        playerCanSee p | distanceSq state.player.pos p > 12 * 12 = false
        playerCanSee p | otherwise = lineOfSight state.level state.player.pos p

        drawTile :: forall a. (String -> String -> a) -> Maybe Tile -> a
        drawTile d (Just Air)        = d (fromCode 176) "002456"
        drawTile d (Just Ground)     = d (fromCode 176) "AAAAAA"
        drawTile d (Just Grass)      = d (fromCode 176) "009900"
        drawTile d (Just Wall)       = d (fromCode 219) "444422"
        drawTile d (Just SWall)      = d (fromCode 219) "444422"
        drawTile d (Just DoorLocked) = d "+" "661111"
        drawTile d (Just DoorClosed) = d "+" "666633"
        drawTile d (Just DoorOpen)   = d "|" "666633"
        drawTile d (Just BgCave)     = d (fromCode 176) "484848"
        drawTile d (Just BgHouse)    = d (fromCode 219) "222205"
        drawTile d (Just Bush)       = d (fromCode 172) "009900"
        drawTile d (Just Water)      = d (fromCode 247) "0000FF"
        drawTile d (Just Stairs)     = d "<" "FFFFFF"
        drawTile d _                 = d "?" "FFFFFF"

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
        updateCreature i game | get i game 0 (\c -> c.time) >= get i game 0 (calcSpeed game []) =
            updateCreature i (modifyCreatureAt i (updateCreatureOnce i game) (\c -> c { time = c.time - calcSpeed game [] c }))
        updateCreature i game | otherwise = game

        -- Creature at index i does a single action.
        updateCreatureOnce :: Number -> GameState -> GameState
        updateCreatureOnce i game = modifyCreatureAt i game $ \c -> updateAI c --updatePhysics game
            where
                updateAI c = case head (pathToPlayer game c.pos) of
                                Just p  -> c { pos = p }
                                Nothing -> c

        pathToPlayer :: GameState -> Point -> [Point]
        pathToPlayer (Game state) start = findPath state.pathfinder start (groundProject state.player.pos)
            where
                -- Projects position to ground or climbable tile.
                groundProject :: Point -> Point
                groundProject p | isClimbable state.level (p .+. {x:0, y:0}) = p
                groundProject p | isValidMove state.level (p .+. {x:0, y:1}) = groundProject (p .+. {x:0, y:1})
                groundProject p | otherwise = p

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
updatePhysics _ c = c { vel = zerop }

move :: forall r. GameState -> { pos :: Point | r } -> Point -> { pos :: Point | r }
move (Game state) c delta =
    if blocked then c else c { pos = newpos }
    where
        newpos  = clampPos $ c.pos .+. delta
        blocked = not $ isValidMove state.level newpos
        w (Level level) = level.width
        h (Level level) = level.height

        clampPos :: Point -> Point
        clampPos pos = { x: clamp pos.x 0 (w state.level - 1), y: clamp pos.y 0 (h state.level - 1) }

updateWorld :: Boolean -> Number -> GameState -> GameState
updateWorld updatePlayer advance =
    updateCreatures advance >>> (if updatePlayer then updatePlayerPhysics else id) >>> updateItemPhysics
    where
        updatePlayerPhysics (Game state) | isClimbable state.level state.player.pos =
            Game state
        updatePlayerPhysics (Game state) | otherwise =
            Game state { player = updatePhysics (Game state) state.player }

        updateItemPhysics (Game state) = Game state { items = map (updatePhysics (Game state)) state.items }

setTile' :: Point -> Tile -> GameState -> GameState
setTile' p t (Game state) = let newlevel = setTile state.level p t
                            in Game state { level = newlevel, pathfinder = makePathfinder (levelWeights newlevel) }

inFreeFall :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
inFreeFall level c = isValidMove level (c.pos .+. {x:0, y: 1}) || c.vel.y < 0

canGrab :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
canGrab level c = climb || (isFree {x:0, y: 1} && (ledge (-1) || ledge 1))
    where
        climb = isClimbable level c.pos
        ledge dx = isFree {x: dx, y: -1} && blocked {x: dx, y: 0}
        isFree delta  = isValidMove level (c.pos .+. delta)
        blocked = not <<< isFree

playerCannotAct :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
playerCannotAct level c = inFreeFall level c && not (canGrab level c)

isValidMove :: Level -> Point -> Boolean
isValidMove level = not <<< isTileSolid <<< fromMaybe Air <<< getTile level

isClimbable :: Level -> Point -> Boolean
isClimbable level = isTileClimbable <<< fromMaybe Air <<< getTile level

carryingWeight :: [Item] -> Number
carryingWeight [] = 0
carryingWeight (x:xs) = x.weight + (carryingWeight xs)

maxCarryingCapacity :: Creature -> Number
maxCarryingCapacity c = c.stats.str * 5 + 10

calcSpeed :: GameState -> [Item] -> Creature -> Number
calcSpeed (Game state) inv c | isClimbable state.level c.pos = 1500
calcSpeed (Game state) inv c | inFreeFall state.level c      = 500
calcSpeed (Game state) []  c                                 = 1000 - (c.stats.dex - 10) * 25
calcSpeed (Game state) inv c | otherwise                     = 1000 - (c.stats.dex - 10) * 25 + (deltaWeight (carryingWeight inv / maxCarryingCapacity c))
    where
        deltaWeight :: Number -> Number
        deltaWeight n | n < 40.0 = 0
        deltaWeight n | n < 60.0 = 50
        deltaWeight n | n < 80.0 = 150
        deltaWeight n | n < 90.0 = 200
        deltaWeight n | n < 100.0 = 400
        deltaWeight n | otherwise = 1000

movePlayer :: Point -> GameState -> GameState
movePlayer delta g@(Game state) =
    if canMove then
        updateWorld false (calcSpeed g state.inventory state.player) $ Game state { player = move (Game state) state.player { vel = zerop } delta }
        else checkTile <<< fromMaybe Air <<< getTile state.level $ newpos
    where
        newpos  = state.player.pos .+. delta
        canMove = isValidMove state.level newpos

        checkTile :: Tile -> GameState
        checkTile DoorClosed = addMsg "You open the door." $ setTile' newpos DoorOpen g
        checkTile DoorLocked = addMsg "The door is locked. You pick the lock." $ setTile' newpos DoorClosed g
        checkTile _          = g

playerJump :: GameState -> Number -> GameState
playerJump g@(Game state) xdir | not (isValidMove state.level (state.player.pos .+. {x: xdir, y: -1})) =
    movePlayer {x: xdir, y: -1} g
playerJump g@(Game state) xdir | isValidMove state.level (state.player.pos .+. {x: xdir, y: -1}) && not (isValidMove state.level (state.player.pos .+. {x: xdir, y: 0})) =
    movePlayer {x: xdir, y: -1} g
playerJump g@(Game state) xdir | isClimbable state.level (state.player.pos .+. {x: xdir, y: -1}) =
    movePlayer {x: xdir, y: -1} g
playerJump g@(Game state) xdir | otherwise =
    Game state { player = state.player { vel = {x: xdir, y: -3} } }

movementkeys :: M.Map Number Point
movementkeys = M.fromList [numpad 8 // {x:  0, y: -1}
                          ,numpad 2 // {x:  0, y:  1}
                          ,numpad 4 // {x: -1, y:  0}
                          ,numpad 6 // {x:  1, y:  0}
                          ,numpad 5 // {x:  0, y:  0}
                          ,numpad 1 // {x: -1, y:  1}
                          ,numpad 3 // {x:  1, y:  1}]

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
onKeyPress console g@(Game state) _   | playerCannotAct state.level state.player = return g
onKeyPress console (Game state@{window = InventoryW }) key | key == 73 = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{window = GameW })      key | key == 73 = drawGame console $ Game state { window = InventoryW }
onKeyPress console g@(Game state) key | key == numpad 7 = drawGame console $ playerJump g (-1)
onKeyPress console g@(Game state) key | key == numpad 9 = drawGame console $ playerJump g 1
onKeyPress console g@(Game state) key | key == numpad 8 = drawGame console $ playerJump g 0
onKeyPress console g@(Game state) key | key == 80       = drawGame console $ pickUp (state.player.pos) g
onKeyPress console g@(Game state) key =
    case M.lookup key movementkeys of
        Just delta -> drawGame console $ movePlayer delta g
        Nothing    -> return g

onKeyPress console MainMenu key                              | key == 13       = return $ NameCreation { playerName: "" }
onKeyPress console (NameCreation { playerName = pname }) key | key == 13       = return $ CharCreation { playerName: pname }
onKeyPress console (NameCreation { playerName = xs })    key | key == 8        = return $ NameCreation { playerName: (take (strlen xs - 1) xs) }
onKeyPress console (NameCreation { playerName = "" })    key                   = return $ NameCreation { playerName: (fromCharArray [fromCharCode key]) }
onKeyPress console (NameCreation { playerName = xs })    key | strlen xs > 15  = return $ NameCreation { playerName: xs }
onKeyPress console (NameCreation { playerName = xs })    key                   = return $ NameCreation { playerName: (xs ++ (fromCharArray [fromCharCode key])) }
onKeyPress console (CharCreation { playerName = xs })    key | key == 65       = return $ initialState xs
onKeyPress _ st _ = return st


main = J.ready $ withConsole 80 25 MainMenu {onKeyPress: onKeyPress, onUpdate: onUpdate}
