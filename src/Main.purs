module Main where

import Data.Char
import Data.String hiding (length, drop)
import Data.Maybe
import Data.Tuple
import Data.Array ((!!), (..), map, updateAt, modifyAt, range, length, head, filter, drop, deleteAt, delete)
import Data.Foldable
import qualified Data.Map as M
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace
import Math

import Graphics.CanvasConsole
import RandomGen
import GameState
import GameData
import Drawing
import Utils
import Level
import Astar
import Line

strlen = Data.String.length

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


-- Updates blinking timer.
updateBlinkTimer :: Number -> GameState -> GameState
updateBlinkTimer dt (Game state) | state.blinkTimer > 0.5 = Game state { blink = not state.blink, blinkTimer = 0 }
updateBlinkTimer dt (Game state) | otherwise              = Game state { blinkTimer = state.blinkTimer + dt }



-- onUpdate is called as often as possible.
onUpdate :: Console -> Number -> GameState -> ConsoleEff GameState
onUpdate console dt g@(Game state) | playerCannotAct state.level state.player && state.freeFallTimer > 0.1 =
    drawGame console <<< updateBlinkTimer dt <<< updateWorld true (calcSpeed g) $ Game state { freeFallTimer = 0 }
onUpdate console dt g@(Game state) | otherwise =
    blinkDraw console <<< updateBlinkTimer dt $ Game state { freeFallTimer = state.freeFallTimer + dt }
onUpdate console _ g = drawGame console g


-- Gets creature information from creature at index i.
get :: forall a. Number -> GameState -> a -> (Creature -> a) -> a
get i (Game state) d getter = fromMaybe d $ getter <$> state.npcs !! i

-- Modify creature at index i with a function.
modifyCreatureAt :: Number -> GameState -> (Creature -> Creature) -> GameState
modifyCreatureAt i (Game state) f = Game state { npcs = modifyAt i f state.npcs }

-- Updates all npcs.
updateCreatures :: Number -> GameState -> GameState
updateCreatures _       g@(Game state'@{ npcs = [] }) = g
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
                updateAI g (AI (Alert _) _) | get i g Peasant (\c -> c.ctype) == Archer && canSeePlayer = get i g g (flip npcAttack g)
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

-- Get equipped weapon (If playuer has no weapon equipped use broken copper dagger)
playerWeapon :: GameState -> Item
playerWeapon (Game state@{ equipments = eq }) =
    case M.lookup WeaponSlot eq of
        Just item -> item
        Nothing   -> { itemType: Weapon { weaponType: Dagger, material: Copper, prefix: [Broken] }, pos: zerop, vel: zerop }

playerAttack :: Number -> GameState -> GameState
playerAttack i g@(Game state) = let attack = playerHit g
                                in if attack.hit then addMsg ("You hit the " ++ (get i g "" (\c -> show c.ctype)) ++ "!") $ modifyCreatureAt i attack.game (takeDamage (playerWeapon g) state.player)
                                   else addMsg ("You missed the " ++ get i g "" (\c -> show c.ctype) ++ "!") attack.game

npcAttack :: Creature -> GameState -> GameState
npcAttack c g = let attack = npcHit c g
                in if attack.hit then addMsg (show c.ctype ++ " hit you!") <<< (\(Game st) -> Game st { player = takeDamage (npcWeapon c) c st.player }) $ attack.game
                else addMsg (capitalize (show c.ctype) ++ " tried to " ++ attackVerb c.ctype ++ " you, but missed.") attack.game
    where
        attackVerb Archer = "shoot"
        attackVerb _      = "attack"

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
        damage = (weaponStat weapon).damage + statModf attacker.stats.str

takeFallingDamage :: Number -- starting y position
                  -> Number -- ending y position
                  -> Number -- weightRatio: inventory + equipments weight / maxCarryingCapacity weightRatio
                  -> Creature -- player
                  -> Creature 
takeFallingDamage y0 y1 wr pl | y0 - y1 >= 3 = pl { stats = pl.stats { hp = pl.stats.hp - damage } }
    where
        damage = floor ((y0 - y1 - 2) * (1 + wr)) * ((y0 - y1 - 2) * (1 + wr))
takeFallingDamage _ _ _ c = c


-- Moves an object that has position. Checks collisions with solid tiles.
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

-- Remove dead npcs from game
removeDead :: GameState -> GameState
removeDead (Game state@{ npcs = n }) = (Game state { npcs = deadGrind n })
    where 
        deadGrind :: [Creature] -> [Creature]
        deadGrind [] = []
        deadGrind (x:xs) | x.stats.hp <= 0 = deadGrind xs
        deadGrind (x:xs) = x : deadGrind xs

-- Updates the game world.
updateWorld :: Boolean -- Update player also?
            -> Number  -- How many time units the game world advances
            -> GameState -> GameState
updateWorld updatePlayer advance =
    removeDead >>> updateCreatures advance >>> (if updatePlayer then updatePlayerPhysics else id) >>> updateItemPhysics
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

canGrab :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
canGrab level c = climb || (isFree {x:0, y: 1} && (ledge (-1) || ledge 1))
    where
        climb = isClimbable level c.pos
        ledge dx = isFree {x: dx, y: -1} && blocked {x: dx, y: 0}
        isFree delta  = isValidMove level (c.pos .+. delta)
        blocked = not <<< isFree

playerCannotAct :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
playerCannotAct level c = inFreeFall level c && not (canGrab level c)


movePlayer :: Point -> GameState -> GameState
movePlayer delta g@(Game state) = let blockIndex = enemyBlocks state.npcs 0
    in if blockIndex >= 0 then
        updateWorld false (weaponStat $ playerWeapon g).attackSpeed $ playerAttack blockIndex g
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
        Just item -> case ( ( carryingWeight (M.values state.equipments) + carryingWeight state.inventory ) / maxCarryingCapacity state.player ) < 110 of
                         true  -> addMsg ("You acquired: " ++ show item.itemType) $ Game state { items = deleteItem state.items point, inventory = item : state.inventory }
                         false -> addMsg "You can't carry that much weight!" $ Game state
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

-- Remove some equipment in particular slot
removeOldEquip :: EquipmentSlot -> M.Map EquipmentSlot Item -> M.Map EquipmentSlot Item
removeOldEquip slot eq =
    case M.lookup slot eq of
        Just item -> M.delete slot eq
        Nothing   -> eq

-- Equip function (used while in inventory and equipment slot is chosen).
equip :: Number -> GameState -> GameState
equip i g@(Game state@{ equipments = eq, window = InventoryW { equip = (Just slot) }, inventory = inv }) =
    case (!!) inv i of
        Just item -> case isValidEquip item.itemType slot of
                        true  -> Game state { equipments = M.insert slot item eq , inventory = deleteAt i 1 inv, window = EquipW }
                        false -> addMsg ("Can't equip that item " ++ show slot) g
        Nothing -> addMsg "There is no item in there." g
equip _ g = g

--choice :: forall a. [a] -> GameState -> { i :: a, game :: GameState }

weaponMaterialList :: Number -> [Material]
weaponMaterialList lvlnum | lvlnum <= 1 = [Wood, Copper, Iron]
weaponMaterialList lvlnum | lvlnum <= 2 = [Copper, Iron, Steel]
weaponMaterialList lvlnum | lvlnum <= 3 = [Iron, Steel, Titanium]
weaponMaterialList lvlnum | lvlnum <= 4 = [Steel, Titanium]
weaponMaterialList lvlnum | otherwise   = [Steel, Titanium, Adamantine]

armorMaterialList :: Number -> [Material]
armorMaterialList lvlnum | lvlnum <= 1 = [Leather, Copper, Iron]
armorMaterialList lvlnum | lvlnum <= 2 = [Copper, Iron, Steel]
armorMaterialList lvlnum | lvlnum <= 3 = [Iron, Steel, Titanium]
armorMaterialList lvlnum | lvlnum <= 4 = [Steel, Titanium]
armorMaterialList lvlnum | otherwise   = [Steel, Titanium, Adamantine]

randomWeaponMaterial :: GameState -> { i :: Material, game :: GameState }
randomWeaponMaterial g@(Game state) = unsafeChoice (weaponMaterialList state.lvlnum) g

randomArmorMaterial :: GameState -> { i :: Material, game :: GameState }
randomArmorMaterial g@(Game state) = unsafeChoice (weaponMaterialList state.lvlnum) g

randomWeaponPrefix :: GameState -> { i :: WeaponPrefix, game :: GameState }
randomWeaponPrefix = unsafeChoice [Broken, Rusty, Dull, Sharp, Lethal, Masterwork, Light, Balanced, Heavy, Godly]

randomArmorPrefix :: GameState -> { i :: ArmorPrefix, game :: GameState }
randomArmorPrefix = unsafeChoice [BrokenA, RustyA, MasterworkA, LightA, HeavyA, GodlyA]


randomLoot :: Point -> GameState -> { item :: Item, game :: GameState }
randomLoot p g = let r = randInt 10 100 g in { item: { itemType: Loot { value: r.n }, pos: p, vel: zerop }, game: r.game }

--generateItem :: GameState -> { item :: Item, game ::  GameState }
--generateItems g = { item: }

generateItems :: [Point] -> GameState -> GameState
generateItems (x:xs) g = g

onKeyPress :: Console -> GameState -> Number -> ConsoleEff GameState

-- If player has 0 hp left, kill the player (no key can save you!)
-- onKeyPress console (Game state) _ | state.player.stats.hp <= 0 = return $ Death { playerName: state.playerName, points: state.points }
-- In death, press enter to star again
onKeyPress console (Death d) key | key == 13 = return $ MainMenu

onKeyPress console g@(Game state) _   | playerCannotAct state.level state.player = return g

-- Change page in inventory with + and -
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 187 || key == 107) && floor ((length inv) / 10) <= iw.index = drawGame console $ Game state { window = InventoryW iw { index = 0 } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 187 || key == 107)                                          = drawGame console $ Game state { window = InventoryW iw { index = (iw.index + 1) } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 189 || key == 109) && iw.index == 0                         = drawGame console $ Game state { window = InventoryW iw { index = (floor ((length inv) / 10)) } }
onKeyPress console (Game state@{ window = InventoryW iw, inventory = inv }) key | (key == 189 || key == 109)                                          = drawGame console $ Game state { window = InventoryW iw { index = (iw.index - 1) } }

-- Choosing equipment slot in equipment window
onKeyPress console g@(Game state@{ window = EquipW , equipments = eq , inventory = inv }) key | elem key numberkeys =
    case (!!) allEquipmentSlots number of
        Just slot  -> case M.lookup slot eq of
                          Just item -> drawGame console $ (Game state { window = EquipW, equipments = removeOldEquip slot eq, inventory = item : inv })
                          Nothing   -> drawGame console $ (Game state { window = InventoryW { index: 0, command: NoCommand, equip: Just slot } })
        Nothing  -> return g
        where
            number :: Number
            number =
                case M.lookup key numbers of
                    Just n  -> n
                    Nothing -> -1

-- Using command in inventory
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == 68        = drawGame console $ Game state { window = InventoryW { index: iw.index, command: Drop, equip: Nothing } }
onKeyPress console g@(Game state@{ window = InventoryW iw@{ command = Drop, equip = Nothing } }) key | elem key numberkeys =
    case M.lookup key numbers of
        Just number -> drawGame console $ dropItem number (Game state { window = InventoryW iw { command = NoCommand } })
        Nothing     -> return g

-- Equipping item while in inventory window (when equip option is valid)
onKeyPress console g@(Game state@{ window = InventoryW iw@{ equip = slot } }) key | elem key numberkeys =
    case M.lookup key numbers of
        Just number -> drawGame console $ equip number g
        Nothing     -> return g

-- Change game states window with s, i and e (GameW, EquipW and InventoryW) TODO: add skillwindow
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == makeCharCode "I" = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{ window = EquipW  })       key | key == makeCharCode "I" = drawGame console $ Game state { window = InventoryW { index: 0, command: NoCommand, equip: Nothing } }
onKeyPress console (Game state@{ window = SkillW })        key | key == makeCharCode "I" = drawGame console $ Game state { window = InventoryW { index: 0, command: NoCommand, equip: Nothing } }
onKeyPress console (Game state@{ window = GameW })         key | key == makeCharCode "I" = drawGame console $ Game state { window = InventoryW { index: 0, command: NoCommand, equip: Nothing } }
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == makeCharCode "E" = drawGame console $ Game state { window = EquipW }
onKeyPress console (Game state@{ window = EquipW })        key | key == makeCharCode "E" = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{ window = SkillW })        key | key == makeCharCode "E" = drawGame console $ Game state { window = EquipW }
onKeyPress console (Game state@{ window = GameW })         key | key == makeCharCode "E" = drawGame console $ Game state { window = EquipW }
onKeyPress console (Game state@{ window = InventoryW iw }) key | key == makeCharCode "A" = drawGame console $ Game state { window = SkillW }
onKeyPress console (Game state@{ window = EquipW })        key | key == makeCharCode "A" = drawGame console $ Game state { window = SkillW }
onKeyPress console (Game state@{ window = SkillW })        key | key == makeCharCode "A" = drawGame console $ Game state { window = GameW }
onKeyPress console (Game state@{ window = GameW })         key | key == makeCharCode "A" = drawGame console $ Game state { window = SkillW }

-- Esc closes inventory, equipment or skill window and returns the player to the game window
onKeyPress console (Game state) key | key == 27 = drawGame console $ Game state { window = GameW }

-- Change movement mode
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "R" && move == RunMode = drawGame console $ Game state { move = NormalMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "R" && move /= RunMode = drawGame console $ Game state { move = RunMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "S" && move == SneakMode = drawGame console $ Game state { move = NormalMode }
onKeyPress console (Game state@{ move = move }) key | key == makeCharCode "S" && move /= SneakMode = drawGame console $ Game state { move = SneakMode }

onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 7               = drawGame console $ playerJump g (-1)
onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 9               = drawGame console $ playerJump g 1
onKeyPress console g@(Game state@{ window = GameW }) key | key == numpad 8               = drawGame console $ playerJump g 0
onKeyPress console g@(Game state@{ window = GameW }) key | key == makeCharCode "G"       = drawGame console $ pickUp (state.player.pos) g
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

-- Select class in character creation
onKeyPress console (CharCreation { playerName = xs }) key =
    case M.lookup key numbers of
        -- Just 9  -> return $ CharCreation { playerName: xs } -- uncomment to prevent the usage of developer class
        Just i  -> return $ (UseSkillPoints { playerName: xs ++ " " ++ getStartingClassName i, skillPoints: getStartingSP i, skills: defaultSkills, player: getStartingPlayer i })
        Nothing -> return $ CharCreation { playerName: xs }
             where
                getStartingPlayer :: Number -> Creature
                getStartingPlayer i = { pos: {x: 4, y: 3}, dir: zerop, ctype: Player, stats: getStartingStats i, time: 0, vel: zerop, ai: NoAI }
    
                getStartingStats :: Number -> Stats
                getStartingStats i =
                    case (!!) startingStats i of
                        Just s  -> s
                        Nothing -> defaultStats
    
                getStartingSP :: Number -> Number
                getStartingSP i =
                    case (!!) startingSkillpoints i of
                        Just sp -> sp
                        Nothing -> 0

                getStartingClassName :: Number -> String
                getStartingClassName i =
                    case (!!) startingClass i of
                        Just sc -> sc
                        Nothing -> "the cheater!"

-- Using skill points in UseSkillPoints window
onKeyPress console u@(UseSkillPoints { playerName = xs, skillPoints = sp, skills = s, player = pl }) key =
     case M.lookup key numbers of
        Just number -> case sp of
                           1 -> return $ initialState xs (raiseSkills number s) pl
                           _ -> return $ (UseSkillPoints { playerName: xs, skillPoints: sp - 1, skills: raiseSkills number s, player: pl })
        Nothing     -> return u
            where
                raiseSkill :: Number -> [Tuple SkillType Skill] -> [Tuple SkillType Skill]
                raiseSkill i list =
                    case (!!) list i of
                        Just (Tuple st { level = l, prog = p })  -> updateAt i (Tuple st { level: l + 1, prog: p }) list
                        Nothing -> list

                raiseSkills :: Number -> Skills -> Skills
                raiseSkills i skills = M.fromList $ raiseSkill i $ M.toList skills


onKeyPress _ st _ = return st


main = J.ready $ withConsole 80 25 MainMenu {onKeyPress: onKeyPress, onUpdate: onUpdate}
