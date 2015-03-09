module GameData where

import Data.String
import Utils

type Point = { x :: Number, y :: Number }

(.+.) :: Point -> Point -> Point
(.+.) {x = x1, y = y1} {x = x2, y=y2} = {x: x1+x2, y: y1+y2}
infixl 9 .+.

(.==.) :: Point -> Point -> Boolean
(.==.) {x = x1, y = y1} {x = x2, y = y2} = x1 == x2 && y1 == y2

zerop :: Point
zerop = {x: 0, y: 0}

unitp :: Point -> Point
unitp p = {x: sign p.x, y: sign p.y}

type Stats = { hp :: Number }

defaultStats = { hp: 10 }

data CreatureType = Player | Guard | Archer | Peasant

instance showCT :: Show CreatureType where
    show Player = "player"
    show Guard   = "guard"
    show Archer  = "archer"
    show Peasant = "peasant"

instance eqCT :: Eq CreatureType where
    (==) a b = show a == show b
    (/=) a b = show a /= show b


type Creature =
    { pos   :: Point
    , ctype :: CreatureType
    , stats :: Stats
    , speed :: Number
    , time  :: Number
    , vel   :: Point
    }

data ItemType = Loot   { value :: Number }
              | Weapon { dmg :: Number, attackBonus :: Number }

type Item = { itemType :: ItemType, pos :: Point, vel :: Point, weight :: Number }

