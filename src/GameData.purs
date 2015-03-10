module GameData where

import Data.Tuple
import Data.String
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

data ItemType = Loot   { value :: Number }
              | Weapon { dmg :: Number, attackBonus :: Number }

type Item = { itemType :: ItemType, pos :: Point, vel :: Point, weight :: Number }

showItem :: Item -> String
showItem { itemType = Weapon { dmg = d, attackBonus = ab }, weight = w } = "Weapon, Dmg: " ++ (show d) ++ ", Weight: " ++ (show w)
showItem _ = "Empty"
