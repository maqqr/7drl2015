module GameState where

import Data.Maybe
import Data.Array (length)
import qualified Data.Map as M
import Math

import GameData
import Level
import Astar
import Utils

-- Maximum message buffer size.
messageBufSize :: Number
messageBufSize = 4

-- Adds message to the message buffer.
addMsg :: String -> GameState -> GameState
addMsg msg (Game state) | length state.messageBuf >= messageBufSize =
    Game state { messageBuf = msg : Data.Array.take (messageBufSize - 1) state.messageBuf }
addMsg msg (Game state) | otherwise =Game state { messageBuf = msg : state.messageBuf }

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
                | InventoryW { index :: Number, command :: InventoryCommand , equip :: Maybe EquipmentSlot }
                | SkillW

data GameState = Game { level         :: Level
                      , player        :: Creature
                      , npcs          :: [Creature]
                      , items         :: [Item]
                      , playerName    :: String
                      , points        :: Number -- Value of stolen loot.
                      , skills        :: Skills
                      , inventory     :: [Item]
                      , equipments    :: M.Map EquipmentSlot Item
                      , freeFallTimer :: Number
                      , messageBuf    :: [String]
                      , pathfinder    :: Pathfinder
                      , window        :: GameWindow
                      , seed          :: Number  -- Seed for random number generator.
                      , blinkTimer    :: Number
                      , blink         :: Boolean -- Blinking indicators are drawn when true.
                      , move          :: MovementMode
                      , lvlnum        :: Number -- Number of levels played
                      }
               | MainMenu
               | NameCreation { playerName :: String }
               | CharCreation { playerName :: String }
               | Death { playerName :: String, points :: Number }

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
        , equipments: M.fromList []
        , freeFallTimer: 0
        , messageBuf: []
        , pathfinder: makePathfinder (levelWeights lvl)
        , window: GameW
        , seed: 456977
        , blinkTimer: 0
        , blink: false
        , move: NormalMode
        , lvlnum: 0
        }
    where
        lvl = stringToLevel castleLevel

        pl = { pos: {x: 4, y: 3}, dir: zerop, ctype: Player, stats: defaultStats, time: 0, vel: zerop, ai: NoAI }

        testGuard = { pos: {x: 17, y: 16}, dir:zerop, ctype: Archer, stats: defaultStats, time: 0, vel: zerop, ai: AI NoAlert (Idle {x: 10, y: 20}) }

        testItem1 = { itemType: Weapon { weaponType: Sword, material: Iron, prefix: [Masterwork] }, pos: {x: 6, y: 4}, vel: {x: 0, y: 0} }
        testItem2 = { itemType: Loot { value: 3 }, pos: {x: 20, y: 3}, vel: {x: 0, y: 0} }
        testItem3 = { itemType: Weapon { weaponType: Axe, material: Steel, prefix: [Rusty] }, pos: {x: 40, y: 4}, vel: {x: 0, y: 0} }
        testItem4 = { itemType: Loot { value: 3 }, pos: {x: 5, y: 3}, vel: {x: 0, y: 0} }


data EquipmentSlot = WeaponSlot | ShieldSlot | ArmorSlot | RingSlot

instance showEquipmentSlot :: Show EquipmentSlot where
    show WeaponSlot = "in weapon hand"
    show ShieldSlot = "in shield hand"
    show ArmorSlot  = "on body"
    show RingSlot   = "in finger"

instance eqEquipmentSlot :: Eq EquipmentSlot where
    (==) a b = show a == show b
    (/=) a b = not (a == b)

instance ordEquipmentSlot :: Ord EquipmentSlot where
    compare a b = compare (show a) (show b)
    
allEquipmentSlots :: [EquipmentSlot]
allEquipmentSlots = [WeaponSlot, ShieldSlot, ArmorSlot, RingSlot]

equipmentsToString :: M.Map EquipmentSlot Item -> String
equipmentsToString m = slotsToStirng 0 allEquipmentSlots m
    where
        slotsToStirng :: Number -> [EquipmentSlot] -> M.Map EquipmentSlot Item -> String
        slotsToStirng _ [] _ = ""
        slotsToStirng 9 _  _ = "" -- More than 10 (0 to 9) equipment slots are not allowed, would cause problems with equipping and unequipping. 
        slotsToStirng i (x:xs) m = 
            case M.lookup x m of
                Just item -> fill ("(" ++ show i ++ "): Item equipped " ++ show x ++ ": ") 35 ++ showItem item ++ "\n\n" ++ slotsToStirng (i + 1) xs m
                Nothing   -> fill ("(" ++ show i ++ "): Item equipped " ++ show x ++ ": ") 35 ++ " Nothing\n\n" ++ slotsToStirng (i + 1) xs m

isValidEquip :: ItemType -> EquipmentSlot -> Boolean
isValidEquip (Weapon w) WeaponSlot = true
isValidEquip _ _                   = false

maxCarryingCapacity :: Creature -> Number
maxCarryingCapacity c = c.stats.str * 5 + 10

speedWithItems :: Creature -> [Item] -> M.Map EquipmentSlot Item -> Number
speedWithItems c inv m = floor $ 1000 - (c.stats.dex - 10) * 25 + (deltaWeight ( ( carryingWeight (M.values m) + carryingWeight inv ) / maxCarryingCapacity c ))
    where
        deltaWeight :: Number -> Number
        deltaWeight x = 0.11 * x * x - 1.01 * x

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
calcSpeed g@(Game state) | otherwise = moveModeModifier g $ speedWithItems state.player state.inventory state.equipments
