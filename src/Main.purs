module Main where

import Data.Char
import Data.String hiding (length, drop)
import Data.Maybe
import Data.Tuple
import Data.Array ((!!), (..), map, updateAt, modifyAt, range, length, head, filter, drop, deleteAt)
import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace
import Math

import Graphics.CanvasConsole
import GameData
import Utils
import Level
import Astar
import Line

strlen = Data.String.length

data MovementMode = NormalMode | SneakMode | RunMode

instance showMovementMode :: Show MovementMode where
    show NormalMode = "normal"
    show SneakMode  = "sneak"
    show RunMode    = "run"

instance eqMovementMode :: Eq MovementMode where
    (==) a b = show a == show b
    (/=) a b = not (a == b)

data InventoryCommand = Drop | Use | NoCommand

data GameWindow = GameW
                | EquipW
                | InventoryW { index :: Number, command :: InventoryCommand }
                | SkillW

data GameState = Game { level         :: Level
                      , player        :: Creature
                      , npcs          :: [Creature]
                      , items         :: [Item]
                      , playerName    :: String
                      , points        :: Number -- Value of stolen loot.
                      , skills        :: Skills
                      , inventory     :: [Item]
                      , freeFallTimer :: Number
                      , messageBuf    :: [String]
                      , pathfinder    :: Pathfinder
                      , window        :: GameWindow
                      , seed          :: Number  -- Seed for random number generator.
                      , blinkTimer    :: Number
                      , blink         :: Boolean -- Blinking indicators are drawn when true.
                      , move          :: MovementMode
                      }
               | MainMenu
               | NameCreation { playerName :: String }
               | CharCreation { playerName :: String }

initialState :: String -> GameState
initialState pname = Game
        { level: lvl
        , player: pl
        , npcs: [testGuard]
        , items: replicate 12 testItem1 ++ [testItem2, testItem3, testItem4]
        , playerName: pname
        , points: 0
        , skills: defaultSkills
        , inventory: []
        , freeFallTimer: 0
        , messageBuf: []
        , pathfinder: makePathfinder (levelWeights lvl)
        , window: GameW
        , seed: 456977
        , blinkTimer: 0
        , blink: false
        , move: NormalMode
        }
    where
        lvl = stringToLevel testLevel

        pl = { pos: {x: 4, y: 3}, dir: zerop, ctype: Player, stats: defaultStats, time: 0, vel: zerop, ai: NoAI }

        testGuard = { pos: {x: 10, y: 20}, dir:zerop, ctype: Guard, stats: defaultStats, time: 0, vel: zerop, ai: AI NoAlert (Idle {x: 10, y: 20}) }

        testItem1 = { itemType: Weapon { weaponType: Sword, material: Iron, prefix: [Masterwork] }, pos: {x: 6, y: 4}, vel: {x: 0, y: 0} }
        testItem2 = { itemType: Loot { value: 3 }, pos: {x: 20, y: 3}, vel: {x: 0, y: 0} }
        testItem3 = { itemType: Weapon { weaponType: Axe, material: Steel, prefix: [Rusty] }, pos: {x: 40, y: 4}, vel: {x: 0, y: 0} }
        testItem4 = { itemType: Loot { value: 3 }, pos: {x: 5, y: 3}, vel: {x: 0, y: 0} }

-- Generates random number.
generate :: GameState -> { n :: Number, game :: GameState }
generate (Game state) =
    let new = (a * state.seed + c) % m in { n: new, game: Game state { seed = new } }
    where
        a = 0x343FD
        c = 0x269EC
        m = pow 32 2

-- Generates random integer between min' and max'
randInt :: Number -> Number -> GameState -> { n :: Number, game :: GameState }
randInt min' max' g = let gg = generate g in  gg { n = min' + (gg.n % (max' - min')) }

-- Generates random point.
randomPoint :: GameState -> { p :: Point, game :: GameState }
randomPoint g@(Game { level = (Level level) }) =
    let x = randInt 0 level.width g
        y = randInt 0 level.height x.game
    in { p: {x: x.n, y: y.n}, game: y.game }

-- Maximum message buffer size.
messageBufSize :: Number
messageBufSize = 4

-- Uses skill and gets experience.
useSkill :: SkillType
         -> Number -- Odds of success when unskilled (0 - 99)
         -> Number -- Experience gained
         -> GameState
         -> { success :: Boolean, game :: GameState }
useSkill skillType odds expr g@(Game state) = { success: success, game: addExp gen.game }
    where
        gen       = randInt 0 100 g
        finalOdds = odds * pow 1.2 (fromMaybe 0 $ (\s -> s.level) <$> M.lookup skillType state.skills)
        success   = gen.n < finalOdds
        addExp g@(Game state) | success = -- Skill increases with successes
            let skill        = fromMaybe { level: 0, prog: 0 } $ M.lookup skillType state.skills
                updatedSkill = increaseSkill skill
                gotLevel     = updatedSkill.level > skill.level
            in addLevelUpMsg gotLevel updatedSkill.level $ Game state { skills = M.insert skillType updatedSkill state.skills }
        addExp g@(Game state) | otherwise = g -- No experience gained

        expRequired :: Number -> Number
        expRequired lvl = 100 + 50 * lvl

        addLevelUpMsg :: Boolean -> Number -> GameState -> GameState
        addLevelUpMsg true newlvl = addMsg $ "Your " ++ show skillType ++ " skill increased to " ++ show newlvl ++ "."
        addLevelUpMsg _ _         = id

        increaseSkill :: Skill -> Skill
        increaseSkill skill | skill.prog + expr >= expRequired skill.level =
            skill { level = skill.level + 1, prog = skill.prog + expr - expRequired skill.level }
        increaseSkill skill | otherwise =
            skill { prog = skill.prog + expr }


-- Adds message to the message buffer.
addMsg :: String -> GameState -> GameState
addMsg msg (Game state) | length state.messageBuf >= messageBufSize =
    Game state { messageBuf = msg : Data.Array.take (messageBufSize - 1) state.messageBuf }
addMsg msg (Game state) | otherwise =Game state { messageBuf = msg : state.messageBuf }

-- Draws the message buffer.
drawMessages :: Console -> Point -> Number -> [String] -> ConsoleEff Unit
drawMessages console p col (x:xs) = drawString console x (rgb col col col) p.x p.y
                                 >> drawMessages console {x: p.x, y: p.y - 1} (col - 50) xs
drawMessages _       _ _   []     = return unit

drawStrings :: Console -> Point -> String -> [String] -> ConsoleEff Unit
drawStrings console p col (x:xs) = drawString console x col p.x p.y
                                >> drawStrings console {x: p.x, y: p.y + 1} col xs
drawStrings _ _  _  []           = return unit

-- Updates blinking timer.
updateBlinkTimer :: Number -> GameState -> GameState
updateBlinkTimer dt (Game state) | state.blinkTimer > 0.5 = Game state { blink = not state.blink, blinkTimer = 0 }
updateBlinkTimer dt (Game state) | otherwise              = Game state { blinkTimer = state.blinkTimer + dt }

-- Draws the game when state.blink changes to true or false.
blinkDraw :: Console -> GameState -> ConsoleEff GameState
blinkDraw console g@(Game state) | state.blinkTimer == 0 = drawGame console g
blinkDraw console g@(Game state) | otherwise             = return g

-- onUpdate is called as often as possible.
onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt g@(Game state) | playerCannotAct state.level state.player && state.freeFallTimer > 0.1 =
    drawGame console <<< updateBlinkTimer dt <<< updateWorld true (calcSpeed g) $ Game state { freeFallTimer = 0 }
onUpdate console dt g@(Game state) | otherwise =
    blinkDraw console <<< updateBlinkTimer dt $ Game state { freeFallTimer = state.freeFallTimer + dt }
onUpdate console _ g = drawGame console g

-- Draws the game state.
drawGame :: Console -> GameState -> ConsoleEff GameState
drawGame console g@(Game state@{ window = SkillW }) = do
    clear console
    drawString console ("Name: " ++ state.playerName ++ " Points: " ++ show state.points) "AAAAAA" 2 2
    drawString console (skillsInfo state.skills) "AAAAAA" 2 6
    drawString console (statsToString state.player.stats) "AAAAAA" 3 4
    drawStrings console {x: 1, y: 20} "FFFFFF" state.messageBuf
    return g
drawGame console g@(Game state@{ window = EquipW }) = do
    clear console
    drawString console "Press e to continue and i to open your inventory." "AAAAAA" 1 1
    drawString console "Equipments: " "AAAAAA" 2 4
    --TODO: equipment system
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return g
drawGame console g@(Game state@{ window = InventoryW { index = page , command = com }, inventory = inv }) = do
    clear console
    drawString console "Press i to continue and e to open your equipments. Change page with + and -." "AAAAAA" 1 1
    drawString console ("Inventory (page " ++ show (page + 1) ++ "/" ++ show (floor ((length inv) / 10) + 1) ++ "): (Carrying: " ++ (show $ carryingWeight inv) ++ " lbs)") "AAAAAA" 2 4
    drawInventoryPage inv 0 (page * 10) 4 5
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return g
        where
            drawItemInfo :: Maybe Item -> Number -> Number -> ConsoleEff Unit
            drawItemInfo (Just i) x y = drawString console (showItem i) "AAAAAA" x y
            drawItemInfo _ x y        = drawString console "empty"      "AAAAAA" x y

            drawInventoryPage :: [Item] -> Number -> Number -> Number -> Number -> ConsoleEff Unit
            drawInventoryPage [] num i x y = drawString console "--- Empty ---" "AAAAAA" x y --End recursion when list is empty.
            drawInventoryPage _ 10 _ x y        = drawString console "-------------" "AAAAAA" x y --End recursion when full page (10 items) has been drawn.
            drawInventoryPage items num i x y  = do
                drawItemInfo ((!!) items i) (x + 5) y
                drawString console ("(" ++ show num ++ "): ") "AAAAAA" x y
                drawInventoryPage items (num + 1) (i + 1) x (y + 1)
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
    drawMessages console {x: 1, y: 23} 255 state.messageBuf
    return g
    where
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
        drawCreatureType d Player  = d "@" "FF0000"
        drawCreatureType d Guard   = d "G" "0000FF"
        drawCreatureType d Archer  = d "A" "00FF00"
        drawCreatureType d Peasant = d "P" "AAAAFF"
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

        drawTileWithFov :: forall a. Point -> (String -> String -> a) -> Maybe Tile -> a
        drawTileWithFov p d t | playerCanSee p = drawTile d t
        drawTileWithFov p d t | otherwise      = d (fromCode 178) "111111"

        playerCanSee :: Point -> Boolean
        -- playerCanSee p = true
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
        drawTile d (Just Trunk)      = d "I" "666633"
        drawTile d (Just Leaves)     = d (fromCode 5) "005500"
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


-- Gets creature information from creature at index i.
get :: forall a. Number -> GameState -> a -> (Creature -> a) -> a
get i (Game state) d getter = fromMaybe d $ getter <$> state.npcs !! i

-- Modify creature at index i with a function.
modifyCreatureAt :: Number -> GameState -> (Creature -> Creature) -> GameState
modifyCreatureAt i (Game state) f = Game state { npcs = modifyAt i f state.npcs }

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

        -- Updates creature at index i until it runs out of time.
        updateCreature :: Number -> GameState -> GameState
        updateCreature i game | get i game 0 (\c -> c.time) >= get i game 0 (calcNpcSpeed game) =
            updateCreature i (modifyCreatureAt i (updateCreatureOnce i game) (\c -> c { time = c.time - calcNpcSpeed game c }))
        updateCreature i game | otherwise = game

        -- Creature at index i does a single action.
        updateCreatureOnce :: Number -> GameState -> GameState
        updateCreatureOnce i g@(Game state) =
            let spotRoll     = randInt 0 99 g
                newAlertAI   = updateAlertness spotRoll.n (get i g NoAI (\c -> c.ai))
                changedAlert = modifyCreatureAt i spotRoll.game (\c ->c { ai = newAlertAI })
            in updateAI changedAlert newAlertAI -- add updatePhysics?
            where
                -- Creature position
                cpos :: Point
                cpos = get i g zerop (\c -> c.pos)

                lookingAtPlayer :: Boolean
                lookingAtPlayer | get i g false (\c -> c.dir.x == 0)                               = true
                lookingAtPlayer | get i g false (\c -> c.dir.x < 0) && cpos.x > state.player.pos.x = true
                lookingAtPlayer | get i g false (\c -> c.dir.x > 0) && cpos.x < state.player.pos.x = true
                lookingAtPlayer | otherwise = false

                canSeePlayer :: Boolean
                canSeePlayer = lookingAtPlayer && (distanceSq cpos state.player.pos < 10 * 10) && (lineOfSight state.level cpos state.player.pos)

                canHide :: Number -> Boolean
                canHide roll = roll < (40 * pow 1.2 (fromMaybe 0 $ (\s -> s.level) <$> M.lookup Sneak state.skills))

                updateAlertness :: Number -> AI -> AI
                updateAlertness roll (AI NoAlert st)  | canSeePlayer = AI MightSee st
                updateAlertness roll (AI NoAlert st)  | otherwise    = AI NoAlert st
                updateAlertness roll (AI MightSee st) | canSeePlayer && not (canHide roll) = AI (Suspicious 1) st
                updateAlertness roll (AI MightSee st) | otherwise                          = AI NoAlert st
                updateAlertness roll (AI (Suspicious n) st) | n == 0                = AI NoAlert st
                updateAlertness roll (AI (Suspicious n) st) | not canSeePlayer      = AI (Suspicious (n - 1)) st
                updateAlertness roll (AI (Suspicious n) st) | canSeePlayer && n < 4 = AI (Suspicious (n + 1)) st
                updateAlertness roll (AI (Suspicious n) st) | otherwise             = AI (Alert 10) st
                updateAlertness roll (AI (Alert n) st) | canSeePlayer = AI (Alert 10) st
                updateAlertness roll (AI (Alert n) st) | n == 0       = AI NoAlert st
                updateAlertness roll (AI (Alert n) st) | otherwise    = AI (Alert (n - 1)) st
                updateAlertness _ x = x

                newPatrolPoint :: GameState -> { p :: Point, game :: GameState }
                newPatrolPoint g' = let points = validPatrolPoints
                                        gen    = randInt 0 (length points - 1) g'
                                    in { p: fromMaybe zerop (points !! gen.n), game: gen.game }

                validPatrolPoints :: [Point]
                validPatrolPoints = filter canWalkOn $ levelPoints state.level
                    where
                        canWalkOn p = isValidMove state.level p && not (isValidMove state.level (p .+. {x: 0, y: 1}))

                updateAI :: GameState -> AI -> GameState
                updateAI g (AI (Alert _) _)      = mover g $ moveToPlayer
                updateAI g (AI (Suspicious _) _) = mover g $ moveToPlayer
                updateAI g (AI _ (Idle p))       = mover g $ moveToPoint p
                updateAI g (AI a (Patrol p)) =
                    if length (pathToPoint g cpos p) <= 2 then
                        let gen = newPatrolPoint g
                        in modifyCreatureAt i gen.game (\c -> c { ai = AI a (Patrol gen.p) })
                    else mover g $ moveToPoint p
                updateAI g _ = g

                mover :: GameState -> (Creature -> Point) -> GameState
                mover g@(Game state) f = let nextPoint = get i g zerop f
                            in if state.player.pos .==. nextPoint then
                                -- get doesn't really get anything from creature, I'm just exploiting
                                -- it's type to make the attack.
                                -- get :: Number -> GameState -> GameState -> (Creature -> GameState) -> Gamestate
                                get i g g (flip npcAttack g)
                            else modifyCreatureAt i g $ walk nextPoint

                walk :: Point -> Creature -> Creature
                walk p c = c { pos = p, dir = p .-. c.pos }

                moveToPoint :: Point -> Creature -> Point
                moveToPoint p c = case head (pathToPoint g c.pos p) of
                                   Just p'  -> p'
                                   Nothing -> c.pos

                moveToPlayer :: Creature -> Point
                moveToPlayer c = case head (pathToPlayer g c.pos) of
                                   Just p'  -> p'
                                   Nothing -> c.pos

        pathToPoint :: GameState -> Point -> Point -> [Point]
        pathToPoint (Game state) start end = findPath state.pathfinder start end

        pathToPlayer :: GameState -> Point -> [Point]
        pathToPlayer g@(Game state) start = pathToPoint g start (groundProject state.player.pos)
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

npcWeapon :: Creature -> Item
npcWeapon _ = { itemType: Weapon { weaponType: Sword, material: Iron, prefix: [Masterwork] }, pos: zerop, vel: zerop }

-- todo: get player weapon from equipment
playerWeapon :: GameState -> Item
playerWeapon _ = { itemType: Weapon { weaponType: Axe, material: Iron, prefix: [Masterwork] }, pos: zerop, vel: zerop }

playerAttack :: Number -> GameState -> GameState
playerAttack i g@(Game state) = let attack = playerHit g
                                in if attack.hit then addMsg ("You hit " ++ (get i g "" (\c -> show c.ctype)) ++ "!") $ modifyCreatureAt i attack.game (takeDamage (playerWeapon g) state.player)
                                   else addMsg ("You missed " ++ get i g "" (\c -> show c.ctype) ++ "!") attack.game

npcAttack :: Creature -> GameState -> GameState
npcAttack c g = let attack = npcHit c g
                in if attack.hit then addMsg (show c.ctype ++ " hit you!") <<< (\(Game st) -> Game st { player = takeDamage (npcWeapon c) c st.player }) $ attack.game
                else addMsg (show c.ctype ++ " tried to attack you, but missed.") attack.game

baseHitChance :: Creature -> Number
baseHitChance c = 50 + 5 * (statModf c.stats.dex)

playerHit :: GameState -> { hit :: Boolean, game :: GameState }
playerHit g@(Game state) = let u = useSkill WeaponSkill (baseHitChance state.player) 20 g
                           in { hit: u.success, game: u.game }

npcHit :: Creature -> GameState -> { hit :: Boolean, game :: GameState }
npcHit c g = let u = randInt 0 99 g in { hit: u.n < baseHitChance c, game: u.game }

takeDamage :: Item -> Creature -> Creature -> Creature
takeDamage weapon attacker defender = defender { stats = defender.stats { hp = defender.stats.hp - damage } }
    where
        damage = (itemStat weapon).damage + statModf attacker.stats.str


-- Moves an object with position. Checks collisions with solid tiles.
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

-- Updates the game world.
updateWorld :: Boolean -- Update player also?
            -> Number  -- How many time units the game world advances
            -> GameState -> GameState
updateWorld updatePlayer advance =
    updateCreatures advance >>> (if updatePlayer then updatePlayerPhysics else id) >>> updateItemPhysics
    where
        updatePlayerPhysics (Game state) | isClimbable state.level state.player.pos =
            Game state
        updatePlayerPhysics (Game state) | otherwise =
            Game state { player = updatePhysics (Game state) state.player }

        updateItemPhysics (Game state) = Game state { items = map (updatePhysics (Game state)) state.items }

-- Sets tile at point p to tile t. Recalculates pathfinding graph.
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
carryingWeight (x:xs) = (itemStat x).weight + (carryingWeight xs)

maxCarryingCapacity :: Creature -> Number
maxCarryingCapacity c = c.stats.str * 5 + 10

speedWithItems c inv = 1000 - (c.stats.dex - 10) * 25 + (deltaWeight (carryingWeight inv / maxCarryingCapacity c))
    where
        deltaWeight :: Number -> Number
        deltaWeight n | n < 40.0 = 0
        deltaWeight n | n < 60.0 = 50
        deltaWeight n | n < 80.0 = 150
        deltaWeight n | n < 90.0 = 200
        deltaWeight n | n < 100.0 = 400
        deltaWeight n | otherwise = 1000

calcNpcSpeed :: GameState -> Creature -> Number
calcNpcSpeed (Game state) c | isClimbable state.level c.pos && isValidMove state.level (c.pos .+. {x: 0, y: 1}) = 1500
calcNpcSpeed (Game state) c | inFreeFall state.level c = 500
calcNpcSpeed (Game state) c | otherwise = 1000

-- Movement speed modifier when only sneak affects it.
sneakSpeedModifier :: GameState -> Number -> Number
sneakSpeedModifier (Game { move = SneakMode }) speed = floor (speed * 2)
sneakSpeedModifier _                           speed = speed

-- Movement speed modifier.
moveModeModifier g@(Game { move = SneakMode }) speed = sneakSpeedModifier g speed
moveModeModifier (Game { move = RunMode })   speed = floor (speed / 1.5)
moveModeModifier _                           speed = speed

-- Calculates speed value for player.
calcSpeed :: GameState -> Number
calcSpeed g@(Game state) | isClimbable state.level state.player.pos && isValidMove state.level (state.player.pos .+. {x: 0, y: 1}) = sneakSpeedModifier g 1500
calcSpeed g@(Game state) | inFreeFall state.level state.player = 500
calcSpeed g@(Game state) | otherwise = moveModeModifier g $ speedWithItems state.player state.inventory

movePlayer :: Point -> GameState -> GameState
movePlayer delta g@(Game state) = let blockIndex = enemyBlocks state.npcs 0
    in if blockIndex >= 0 then
        updateWorld false (itemStat $ playerWeapon g).attackSpeed $ playerAttack blockIndex g
        else if canMove then
            updateWorld false (calcSpeed g) <<< useMoveSkill $ Game state { player = move (Game state) state.player { vel = zerop } delta }
            else checkTile <<< fromMaybe Air <<< getTile state.level $ newpos
    where
        useMoveSkill :: GameState -> GameState
        useMoveSkill g@(Game state) | state.move == RunMode && not (delta .==. zerop) = (useSkill Athletics 50 2 g).game
        useMoveSkill g@(Game state) | state.move == SneakMode && canGetSneakSkill g && not (delta .==. zerop) = (useSkill Sneak 100 5 g).game
        useMoveSkill g@(Game state) | otherwise = g

        canGetSneakSkill :: GameState -> Boolean
        canGetSneakSkill (Game state) = any (\c -> lineOfSight state.level c.pos state.player.pos) $ filter (\c -> noAlert c.ai) state.npcs

        noAlert :: AI -> Boolean
        noAlert (AI NoAlert _)  = true
        noAlert (AI MightSee _) = true
        noAlert _               = false

        newpos  = state.player.pos .+. delta
        canMove = isValidMove state.level newpos

        enemyBlocks (c:cs) i | c.pos .==. newpos = i
        enemyBlocks (c:cs) i | otherwise         = enemyBlocks cs (i + 1)
        enemyBlocks [] _                         = -1

        checkTile :: Tile -> GameState
        checkTile DoorClosed = updateWorld false 500 $ addMsg "You open the door." $ setTile' newpos DoorOpen g
        checkTile DoorLocked = updateWorld false 2000 $ pickLock
        checkTile _          = g

        pickLock :: GameState
        pickLock = let use = useSkill Lockpick 10 20 g
                   in if use.success then addMsg "The door is locked. You managed to pick the lock." $ setTile' newpos DoorClosed use.game
                   else addMsg "The door is locked. You fail to pick the lock." use.game

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

numberkeys :: [Number]
numberkeys = (48 .. 57) ++ (96 .. 105)

numbers :: M.Map Number Number
numbers = M.fromList [48 // 0, 49 // 1, 50 // 2, 51 // 3, 52  // 4, 53  // 5, 54  // 6, 55  // 7, 56  // 8, 57  // 9
                     ,96 // 0, 97 // 1, 98 // 2, 99 // 3, 100 // 4, 101 // 5, 102 // 6, 103 // 7, 104 // 8, 105 // 9]

-- Player tries to pick up an item.
pickUp :: Point -> GameState -> GameState
pickUp point (Game state) =
    case head (filter (\i -> i.pos .==. point) state.items) of
        Just ( { itemType = Loot { value = points } } ) -> addMsg ("You found " ++ show points ++ " $!") $ Game state { items = deleteItem state.items point, points = state.points + points }
        Just item -> addMsg ("You acquired: " ++ show item.itemType) $ Game state { items = deleteItem state.items point, inventory = item : state.inventory }
        Nothing   -> Game state
    where
        deleteItem :: [Item] -> Point -> [Item]
        deleteItem (x:xs) delPos | x.pos .==. delPos = xs
        deleteItem (x:xs) delPos | otherwise = x : deleteItem xs delPos
        deleteItem [] _ = []

-- Player tries to drop item while in inventory.
dropItem :: Number -> GameState -> GameState
dropItem itemNumber g@(Game state@{ window = InventoryW wi, inventory = inv, items = itemsOnGround }) = 
    case (!!) inv i of
        (Just item) -> addMsg ("You dropped: " ++ show item.itemType) $ (Game state { inventory = deleteAt i 1 inv, items = (item { pos = state.player.pos }) : itemsOnGround, window = InventoryW wi { index = 0 } })
        Nothing -> g
    where
        i = itemNumber + wi.index * 10
dropItem _ g = g

onKeyPress :: Console -> GameState -> Number -> ConsoleEff GameState
onKeyPress console g@(Game state) _   | playerCannotAct state.level state.player = return g

-- Change page in inventory with + and -
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 187 || key == 107) && floor ((length inv) / 10) <= iw.index = drawGame console $ Game state { window = InventoryW iw { index = 0 } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 187 || key == 107)                                          = drawGame console $ Game state { window = InventoryW iw { index = (iw.index + 1) } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 189 || key == 109) && iw.index == 0                         = drawGame console $ Game state { window = InventoryW iw { index = (floor ((length inv) / 10)) } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 189 || key == 109)                                          = drawGame console $ Game state { window = InventoryW iw { index = (iw.index - 1) } }

-- Using command in inventory
onKeyPress console (Game state@{ window = InventoryW iw })                      key | key == 68           = drawGame console $ addMsg "Which item do you want to drop?" (Game state { window = InventoryW { index: iw.index, command: Drop } })
onKeyPress console g@(Game state@{ window = InventoryW iw@{ command = Drop } }) key | elem key numberkeys =
    case M.lookup key numbers of
        Just number -> drawGame console $ dropItem number (Game state { window = InventoryW iw { command = NoCommand } })
        Nothing     -> return g

-- Change game states window with i and e (GameW, EquipW and InventoryW)
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == 73  = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{ window = GameW })        key  | key == 73  = drawGame console $ Game state { window = InventoryW { index: 0, command: NoCommand } }
onKeyPress console (Game state@{ window = EquipW })       key  | key == 69  = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{ window = GameW })        key  | key == 69  = drawGame console $ Game state { window = EquipW }
onKeyPress console (Game state@{ window = EquipW  })      key  | key == 73  = drawGame console $ Game state { window = InventoryW { index: 0, command: NoCommand } }
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == 69  = drawGame console $ Game state { window = EquipW }

-- Open skill window with s (and close it with s)
onKeyPress console (Game state@{ window = GameW })  key | key == makeCharCode "A" = drawGame console $ Game state { window = SkillW }
onKeyPress console (Game state@{ window = SkillW }) key | key == makeCharCode "A" = drawGame console $ Game state { window = GameW }

-- Change movement mode
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "R" && move == RunMode = drawGame console $ Game state { move = NormalMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "R" && move /= RunMode = drawGame console $ Game state { move = RunMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "S" && move == SneakMode = drawGame console $ Game state { move = NormalMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "S" && move /= SneakMode = drawGame console $ Game state { move = SneakMode }

onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 7 = drawGame console $ playerJump g (-1)
onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 9 = drawGame console $ playerJump g 1
onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 8 = drawGame console $ playerJump g 0
onKeyPress console g@(Game state@{ window = GameW }) key | key == 80       = drawGame console $ pickUp (state.player.pos) g
onKeyPress console g@(Game state@{ window = GameW }) key =
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
