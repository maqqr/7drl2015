module GameData where

import Data.Tuple
import Data.Array (map)
import Data.String
import Data.Monoid
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
    { maxHp :: Number
    , hp    :: Number 
    , def   :: Number
    , str   :: Number
    , dex   :: Number
    , int   :: Number
    }

defaultStats = { maxHp: 10, hp: 10, def: 10, str: 10, dex: 10, int: 10}

statsToString :: Stats -> String
statsToString { maxHp = mh, hp  = h, def = de, str = s, dex = d, int = i } = "Hp: " ++ show h ++ "/" ++ show mh ++ ". Defence: " ++ show de ++ ". Str: " ++ show s ++ ". Dex: " ++ show d ++ ". Int: " ++ show i ++"."
statsToString _ = ""

statModf :: Number -> Number
statModf x = -5 + floor (x / 2)

data SkillType = WeaponSkill | Sneak | Athletics | Lockpick

type Skill = { level :: Number, prog :: Number }

type Skills = M.Map SkillType Skill

instance showSkillType :: Show SkillType where
    show WeaponSkill = "weapon"
    show Sneak       = "sneaking"
    show Athletics   = "athletics"
    show Lockpick    = "lockpicking"

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

skillsInfo :: Skills -> String
skillsInfo skills = skillsToString $ M.toList skills
    where
        skillToString :: Tuple SkillType Skill -> String
        skillToString (Tuple sType { level = l, prog = p }) = fill (show sType ++ " skill. ") 21 ++ fill (" Level: " ++ show l ++ ".") 11 ++ " Progress (0 - " ++ show (100 + l * 50) ++ "): " ++ show p ++"\n"

        skillsToString :: [Tuple SkillType Skill] -> String
        skillsToString [] = ""
        skillsToString (x:xs) = skillToString x ++ ("\n") ++ skillsToString xs

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

--------- WEAPON STAT ---------

type WeaponStatRecord = { damage :: Number, weight :: Number, attackSpeed :: Number, attackBonus :: Number }

data WeaponStat = WeaponStat WeaponStatRecord

extractStat :: WeaponStat -> WeaponStatRecord
extractStat (WeaponStat s) = s

instance semigroupWeaponStat :: Semigroup WeaponStat where
    (<>) (WeaponStat a) (WeaponStat b) =
        WeaponStat { damage: a.damage + b.damage
                   , weight: a.weight + b.weight
                   , attackSpeed: a.attackSpeed + b.attackSpeed
                   , attackBonus: a.attackBonus + b.attackBonus
                   }

instance monoidWeaponStat :: Monoid WeaponStat where
    mempty = WeaponStat { damage: 0, weight: 0, attackSpeed: 0, attackBonus: 0 }

--------- MATERIALS ---------

data Material = Wood | Copper | Iron | Steel | Titanium | Adamantine

materialStat :: Material -> WeaponStat
materialStat Wood = WeaponStat { damage: -2, weight: -1, attackSpeed: 1, attackBonus: 0 }
materialStat _ = mempty

--------- WEAPON PREFIXES ---------

data WeaponPrefix = Broken | Rusty | Dull | Sharp | Lethal | Masterwork | Light | Balanced | Heavy | Godly

prefixStat :: WeaponPrefix -> WeaponStat
prefixStat Broken     = WeaponStat { damage: -3, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Rusty      = WeaponStat { damage: -2, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Dull       = WeaponStat { damage: -1, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Sharp      = WeaponStat { damage:  1, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Lethal     = WeaponStat { damage:  2, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Masterwork = WeaponStat { damage:  3, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat Godly      = WeaponStat { damage:  4, weight: 0, attackSpeed: 0, attackBonus: 0 }
prefixStat _          = mempty

showPrefix :: [WeaponPrefix] -> String
showPrefix = joinWith " " <<< map show

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

--------- WEAPON TYPES ---------

data WeaponType = Sword | Axe | Dagger

instance showWeaponType :: Show WeaponType where
    show Sword  = "sword"
    show Axe    = "axe"
    show Dagger = "dagger"

weaponTypeStat :: WeaponType -> WeaponStat
weaponTypeStat Sword  = WeaponStat { damage: 3, weight: 1, attackSpeed: 800,  attackBonus: 1 }
weaponTypeStat Axe    = WeaponStat { damage: 5, weight: 3, attackSpeed: 1200, attackBonus: 0 }
weaponTypeStat Dagger = WeaponStat { damage: 1, weight: 1, attackSpeed: 600,  attackBonus: 2 }

--------- ITEM TYPES ---------

data ItemType = Loot   { value :: Number }
              | Weapon { weaponType :: WeaponType, material :: Material, prefix :: [WeaponPrefix] }

instance showItemType :: Show ItemType where
    show (Loot   { value = val })                         = show val ++ " $"
    show (Weapon { weaponType = t, prefix = [] })         = show t
    show (Weapon { weaponType = t, prefix = prefixList }) = showPrefix prefixList ++ " " ++ show t


--------- ITEM ---------

type Item = { itemType :: ItemType, pos :: Point, vel :: Point }

carryingWeight :: [Item] -> Number
carryingWeight [] = 0
carryingWeight (x:xs) = (itemStat x).weight + (carryingWeight xs)

itemStat :: Item -> WeaponStatRecord
itemStat { itemType = Weapon w } = extractStat (materialStat w.material <> weaponTypeStat w.weaponType <> (monoidSum <<< map prefixStat $ w.prefix))
itemStat _ = extractStat mempty

showItem :: Item -> String
showItem i@{ itemType = Weapon _ } = let s = itemStat i in show i.itemType ++ ", Dmg: " ++ show (s.damage) ++ ", Weight: " ++ show (s.weight)
showItem i = show i.itemType
