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
    , str   :: Number
    , dex   :: Number
    , int   :: Number
    }

defaultStats :: Stats
defaultStats = { maxHp: 10, hp: 10, str: 10, dex: 10, int: 10 }

guardStats :: Stats
guardStats = { maxHp: 20, hp: 20, str: 8, dex: 8, int: 10 }

archerStats :: Stats
archerStats = { maxHp: 10, hp: 10, str: 6, dex: 10, int: 10 }

statsToString :: Stats -> String
statsToString { maxHp = mh, hp  = h, str = s, dex = d, int = i } = "Hp: " ++ show h ++ "/" ++ show mh ++ ". Str: " ++ show s ++ ". Dex: " ++ show d ++ ". Int: " ++ show i ++"."
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

startingClass :: [String]
startingClass = ["the knight", "the monk", "the ninja", "the peasant", "the rogue", "the scholar", "the skillmaster", "the soldier", "the developer"]

startingStats :: [Stats]
startingStats = [ { maxHp: 10, hp: 10, str: 14, dex:  9, int: 10 } -- Knight
                , { maxHp: 10, hp: 10, str: 10, dex: 11, int: 12 } -- Monk 
                , { maxHp: 10, hp: 10, str:  9, dex: 14, int: 10 } -- Ninja 
                , { maxHp:  9, hp:  9, str:  9, dex:  9, int:  9 } -- Peasant 
                , { maxHp: 10, hp: 10, str: 10, dex: 11, int: 11 } -- Rogue
                , { maxHp: 10, hp: 10, str: 09, dex: 11, int: 13 } -- Scholar 
                , { maxHp: 10, hp: 10, str: 10, dex: 10, int: 11 } -- Skillmaster
                , { maxHp: 10, hp: 10, str: 12, dex: 11, int: 10 } -- Soldier 
                , { maxHp: 99, hp: 99, str: 99, dex: 99, int: 99 } -- Developer
                ]

startingSkillpoints :: [Number]
startingSkillpoints = [ 2 -- Knight
                      , 2 -- Monk 
                      , 2 -- Ninja 
                      , 1 -- Peasant 
                      , 5 -- Rogue
                      , 5 -- Scholar 
                      , 8 -- Skillmaster
                      , 2 -- Soldier 
                      , 99 -- Developer
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
    show Player  = "player"
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

--------- ARMOR STAT ---------

type ArmorStatRecord = { defence :: Number, weight :: Number }

data ArmorStat = ArmorStat ArmorStatRecord

extractStatArmor :: ArmorStat -> ArmorStatRecord
extractStatArmor (ArmorStat s) = s

instance semigroupArmorStat :: Semigroup ArmorStat where
    (<>) (ArmorStat a) (ArmorStat b) =
        ArmorStat { defence: a.defence + b.defence
                  , weight: a.weight + b.weight
                  }

instance monoidArmorStat :: Monoid ArmorStat where
    mempty = ArmorStat { defence: 0, weight: 0 }                 

--------- WEAPON STAT ---------

type WeaponStatRecord = { damage :: Number, weight :: Number, attackSpeed :: Number, attackBonus :: Number }

data WeaponStat = WeaponStat WeaponStatRecord

extractStatWeapon :: WeaponStat -> WeaponStatRecord
extractStatWeapon (WeaponStat s) = s

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

data Material = Wood | Copper | Iron | Steel | Titanium | Adamantine | Leather

materialStatWeapon :: Material -> WeaponStat
materialStatWeapon Wood       = WeaponStat { damage: -3, weight: -1, attackSpeed:  -50, attackBonus: 4 }
materialStatWeapon Copper     = WeaponStat { damage: -1, weight:  1, attackSpeed:  100, attackBonus: 0 }
materialStatWeapon Iron       = WeaponStat { damage:  0, weight:  2, attackSpeed:  100, attackBonus: 0 }
materialStatWeapon Steel      = WeaponStat { damage:  1, weight:  3, attackSpeed:  100, attackBonus: 0 }
materialStatWeapon Titanium   = WeaponStat { damage:  2, weight:  4, attackSpeed:    0, attackBonus: 0 }
materialStatWeapon Adamantine = WeaponStat { damage:  3, weight:  4, attackSpeed: -100, attackBonus: 1 }
materialStatWeapon _ = mempty

materialStatArmor :: Material -> ArmorStat
materialStatArmor Leather    = ArmorStat { defence: 1, weight: 10 }
materialStatArmor Copper     = ArmorStat { defence: 2, weight: 30 }
materialStatArmor Iron       = ArmorStat { defence: 3, weight: 35 }
materialStatArmor Steel      = ArmorStat { defence: 4, weight: 45 }
materialStatArmor Titanium   = ArmorStat { defence: 5, weight: 60 }
materialStatArmor Adamantine = ArmorStat { defence: 7, weight: 60 }
materialStatArmor _ = mempty

instance showMaterial :: Show Material where
    show Wood       = "wooden"
    show Leather    = "leather"
    show Copper     = "copper"
    show Iron       = "iron"
    show Steel      = "steel"
    show Titanium   = "titanium"
    show Adamantine = "adamantine"

--------- WEAPON AND ARMOR PREFIXES ---------

data WeaponPrefix = Broken | Rusty | Dull | Sharp | Lethal | Masterwork | Light | Balanced | Heavy | Godly

data ArmorPrefix = BrokenA | RustyA | MasterworkA | LightA | HeavyA | GodlyA

prefixStatWeapon :: WeaponPrefix -> WeaponStat
prefixStatWeapon Broken     = WeaponStat { damage: -3, weight: -1, attackSpeed:   50, attackBonus: -4 }
prefixStatWeapon Rusty      = WeaponStat { damage: -2, weight:  1, attackSpeed:    0, attackBonus: -2 }
prefixStatWeapon Dull       = WeaponStat { damage: -1, weight:  0, attackSpeed:    0, attackBonus: -1 }
prefixStatWeapon Sharp      = WeaponStat { damage:  1, weight:  0, attackSpeed:    0, attackBonus:  1 }
prefixStatWeapon Lethal     = WeaponStat { damage:  2, weight:  0, attackSpeed:    0, attackBonus:  2 }
prefixStatWeapon Masterwork = WeaponStat { damage:  3, weight:  0, attackSpeed: -100, attackBonus:  3 }
prefixStatWeapon Godly      = WeaponStat { damage:  4, weight:  0, attackSpeed: -200, attackBonus:  4 }
prefixStatWeapon Light      = WeaponStat { damage:  0, weight: -2, attackSpeed: -100, attackBonus:  1 }
prefixStatWeapon Balanced   = WeaponStat { damage:  1, weight:  0, attackSpeed: -100, attackBonus:  3 }
prefixStatWeapon Heavy      = WeaponStat { damage:  2, weight:  3, attackSpeed:  100, attackBonus:  0 }
prefixStatWeapon _          = mempty

prefixStatArmor :: ArmorPrefix -> ArmorStat
prefixStatArmor BrokenA     = ArmorStat { defence: -1, weight:  -5 }
prefixStatArmor RustyA      = ArmorStat { defence:  1, weight:   5 }
prefixStatArmor MasterworkA = ArmorStat { defence:  5, weight:   5 }
prefixStatArmor GodlyA      = ArmorStat { defence:  7, weight:  10 }
prefixStatArmor LightA      = ArmorStat { defence:  0, weight: -10 }
prefixStatArmor HeavyA      = ArmorStat { defence:  2, weight:  10 }
prefixStatArmor _          = mempty

showPrefixWeapon :: [WeaponPrefix] -> String
showPrefixWeapon = joinWith " " <<< map show

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

showPrefixArmor :: [ArmorPrefix] -> String
showPrefixArmor = joinWith " " <<< map show

instance showArmorPrefix :: Show ArmorPrefix where
    show RustyA      = "rusty"
    show BrokenA     = "broken"
    show MasterworkA = "masterwork"
    show LightA      = "light"
    show HeavyA      = "heavy"
    show GodlyA      = "godly"

--------- WEAPON TYPES ---------

data WeaponType = Sword | Axe | Dagger | Spear | Mace | BattleAxe

instance showWeaponType :: Show WeaponType where
    show Sword     = "sword"
    show Axe       = "axe"
    show Dagger    = "dagger"
    show Spear     = "spear"'
    show Mace      = "mace"
    show BattleAxe = "battleaxe"

weaponTypeStat :: WeaponType -> WeaponStat
weaponTypeStat Sword     = WeaponStat { damage: 3, weight:  6, attackSpeed:  800, attackBonus: 1 }
weaponTypeStat Axe       = WeaponStat { damage: 5, weight:  8, attackSpeed: 1200, attackBonus: 0 }
weaponTypeStat Dagger    = WeaponStat { damage: 2, weight:  4, attackSpeed:  600, attackBonus: 2 }
weaponTypeStat Spear     = WeaponStat { damage: 2, weight:  4, attackSpeed:  600, attackBonus: 2 }
weaponTypeStat Mace      = WeaponStat { damage: 2, weight:  4, attackSpeed:  600, attackBonus: 2 }
weaponTypeStat BattleAxe = WeaponStat { damage: 9, weight: 10, attackSpeed: 2000, attackBonus: 2 }

--------- ITEM TYPES ---------

data ItemType = Loot   { value :: Number }
              | Weapon { weaponType :: WeaponType, material :: Material, prefix :: [WeaponPrefix] }
              | Armor  { material :: Material, prefix :: [ArmorPrefix]}
              | Shield { material :: Material, prefix :: [ArmorPrefix] }
              | Ring

instance showItemType :: Show ItemType where
    show (Loot   { value = val })                          = "an item worth of " ++ show val ++ " gold coins"
    show (Weapon { weaponType = t, material = mat, prefix  = [] })         = show t
    show (Weapon { weaponType = t, material = mat, prefix  = prefixList }) = showPrefixWeapon prefixList ++ " " ++ show mat ++ " " ++ show t
    show (Armor  { material = mat, prefix = [] })          = show mat ++ " armor"
    show (Armor  { material = mat, prefix = prefixList })  = showPrefixArmor prefixList ++ " " ++ show mat ++ " armor"
    show (Shield  { material = mat, prefix = [] })         = show mat ++ " shield"
    show (Shield  { material = mat, prefix = prefixList }) = showPrefixArmor prefixList ++ " " ++ show mat ++ " shield"
    show Ring                                              = "One ring to fold them and in Haskell bind them."
    show _                                                 = "unknown item"


--------- ITEM ---------

type Item = { itemType :: ItemType, pos :: Point, vel :: Point }

carryingWeight :: [Item] -> Number
carryingWeight [] = 0
carryingWeight (x:xs) =
  case x.itemType of
    Weapon w -> (weaponStat x).weight + (carryingWeight xs)
    Armor  w -> (armorStat  x).weight + (carryingWeight xs)
    Shield w -> (armorStat  x).weight + (carryingWeight xs)
    _        -> carryingWeight xs

weaponStat :: Item -> WeaponStatRecord
weaponStat { itemType = Weapon w } = extractStatWeapon (materialStatWeapon w.material <> weaponTypeStat w.weaponType <> (monoidSum <<< map prefixStatWeapon $ w.prefix))
weaponStat _ = extractStatWeapon mempty

armorStat :: Item -> ArmorStatRecord
armorStat { itemType = Armor  w } = extractStatArmor (materialStatArmor w.material <> (monoidSum <<< map prefixStatArmor $ w.prefix))
armorStat { itemType = Shield w } = extractStatArmor (materialStatArmor w.material <> (monoidSum <<< map prefixStatArmor $ w.prefix))
armorStat _ = extractStatArmor mempty

showItem :: Item -> String
showItem i@{ itemType = Weapon _ } = let s = weaponStat i in show i.itemType ++ ", Dmg: "     ++ show (s.damage)  ++ ", Weight: " ++ show (s.weight)
showItem i@{ itemType = Armor  _ } = let s = armorStat  i in show i.itemType ++ ", Defence: " ++ show (s.defence) ++ ", Weight: " ++ show (s.weight)
showItem i@{ itemType = Shield _ } = let s = armorStat  i in show i.itemType ++ ", Defence: " ++ show (s.defence) ++ ", Weight: " ++ show (s.weight)
showItem i = show i.itemType
