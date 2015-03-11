module GameData where

import Data.Tuple
import Data.Array (map)
import Data.String
import Data.Foldable
import qualified Data.Map as M
import Utils
import Math

type Point = { x :: Number, y :: Number }

(.+.) :: Point -> Point -> Point
(.+.) {x = x1, y = y1} {x = x2, y=y2} = {x: x1+x2, y: y1+y2}
infixl 9 .+.

(.-.) :: Point -> Point -> Point
(.-.) {x = x1, y = y1} {x = x2, y=y2} = {x: x1-x2, y: y1-y2}
infixl 9 .-.

(.==.) :: Point -> Point -> Boolean
(.==.) {x = x1, y = y1} {x = x2, y = y2} = x1 == x2 && y1 == y2

showPoint :: Point -> String
showPoint p = "(" ++ show p.x ++ ", " ++ show p.y ++ ")"

manhattanDistance :: Point -> Point -> Number
manhattanDistance {x = x1, y = y1} {x = x2, y=y2} = abs (x1 - x2) + abs (y1 - y2)

distanceSq :: Point -> Point -> Number
distanceSq {x = x1, y = y1} {x = x2, y=y2} = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

zerop :: Point
zerop = {x: 0, y: 0}

unitp :: Point -> Point
unitp p = {x: sign p.x, y: sign p.y}

type Stats = 
    { hp  :: Number 
    , def :: Number
    , str :: Number
    , dex :: Number
    , int :: Number
    }

defaultStats = { hp: 10, def: 10, str: 10, dex: 10, int: 10}

data SkillType = WeaponSkill | Sneak | Athletics | Lockpick

type Skill = { level :: Number, prog :: Number }

type Skills = M.Map SkillType Skill

instance showSkillType :: Show SkillType where
    show WeaponSkill = "weapon"
    show Sneak = "sneaking"
    show Athletics = "athletics"
    show Lockpick = "lockpicking"

instance ordSkillType :: Ord SkillType where
    compare a b = compare (show a) (show b)

instance eqSkillType :: Eq SkillType where
    (==) a b = show a == show b
    (/=) a b = not (a == b)

defaultSkills :: Skills
defaultSkills = M.fromList [Tuple WeaponSkill {level: 0, prog: 0}
                           ,Tuple Sneak { level: 0, prog: 0}
                           ,Tuple Athletics { level: 0, prog: 0}
                           ,Tuple Lockpick { level: 0, prog: 0}
                           ]


data CreatureType = Player | Guard | Archer | Peasant

instance showCT :: Show CreatureType where
    show Player = "player"
    show Guard   = "guard"
    show Archer  = "archer"
    show Peasant = "peasant"

instance eqCT :: Eq CreatureType where
    (==) a b = show a == show b
    (/=) a b = show a /= show b

data AIState = Sleep
             | Idle Point   -- idling position
             | Patrol Point -- where to go

data Alertness = NoAlert  -- Does not see player
               | MightSee -- There is LOS and a chance to turn into Suspicious
               | Suspicious Number -- Number = progress towards alert
               | Alert Number -- Number = number of turns to stay alert

data AI = AI Alertness AIState | NoAI

type Creature =
    { pos   :: Point -- Position
    , dir   :: Point -- Facing direction
    , ctype :: CreatureType
    , stats :: Stats
    , time  :: Number
    , vel   :: Point -- Velocity
    , ai    :: AI
    }

data Material = Wood | Copper | Iron | Steel | Titanium | Adamantine

data WeaponPrefix = Broken | Rusty | Dull | Sharp | Lethal | Masterwork | Light | Balanced | Heavy | Godly

prefixDamageMod :: WeaponPrefix -> Number
prefixDamageMod Broken     = -3
prefixDamageMod Rusty      = -2
prefixDamageMod Dull       = -1
prefixDamageMod Sharp      = 1
prefixDamageMod Lethal     = 2
prefixDamageMod Masterwork = 3
prefixDamageMod Godly      = 4
prefixDamageMod _ = 0

prefixWeightMod :: WeaponPrefix -> Number
prefixWeightMod _ = 0 -- todo

showPrefix :: [WeaponPrefix] -> String
showPrefix []     = ""
showPrefix (x:xs) = show x ++ showPrefix xs

instance showWeaponPrefix :: Show WeaponPrefix where
    show Rusty      = "rusty"
    show Broken     = "broken"
    show Dull       = "dull"
    show Masterwork = "masterwork"
    show Sharp      = "sharp"
    show Lethal     = "lethal"
    show Light      = "light"
    show Balanced   = "balanced"
    show Heavy      = "heavy"
    show Godly      = "godly"

data WeaponType = Sword | Axe | Dagger

instance showWeaponType :: Show WeaponType where
    show Sword  = "sword"
    show Axe    = "axe"
    show Dagger = "dagger"

baseDamage :: WeaponType -> Number
baseDamage Sword = 3
baseDamage Axe   = 5
baseDamage _     = 0

baseAttackBonus :: WeaponType -> Number
baseAttackBonus Sword = 2
baseAttackBonus Axe   = 0
baseAttackBonus _     = 0

data ItemType = Loot   { value :: Number }
              | Weapon { weaponType :: WeaponType, prefix :: [WeaponPrefix] }

instance showItemType :: Show ItemType where
    show (Loot   { value = val })                         = (show val) ++ " $"
    show (Weapon { weaponType = t, prefix = [] })         = show t
    show (Weapon { weaponType = t, prefix = prefixList }) = (showPrefix prefixList) ++ " " ++ (show t)

type Item = { itemType :: ItemType, pos :: Point, vel :: Point, weight :: Number }

-- Final item information functions:

-- todo: calculate weight with prefixes
-- itemWeight = ...

itemDamage :: Item -> Number
itemDamage { itemType = Weapon w } = baseDamage w.weaponType + sum (map prefixDamageMod w.prefix)
itemDamage _ = 0

showItem :: Item -> String
--showItem i@( { w@( Weapon _ ), weight = we } ) = show w  ++ ", Dmg: " ++ show i ++ ", Weight: " ++ show we
-- or (neather works) 
--showItem ( Item { itemType = (Weapon { weaponType = w, prefix = pre }), weight = we } ) = show (Weapon { weaponType = w, prefix = pre }) ++ ", Dmg: " ++ show (itemDamage ( Item { t@{ Weapon w }, weight = we } )) ++ ", Weight: " ++ show we
showItem _ = "Empty"
