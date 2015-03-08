module GameData where

import Data.String

type Point = { x :: Number, y :: Number }

(.+.) :: Point -> Point -> Point
(.+.) {x = x1, y = y1} {x = x2, y=y2} = {x: x1+x2, y: y1+y2}
infixl 9 .+.

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


type Physics =
    { vel :: Point   -- Velocity
    , jump :: Number -- Jump power
    }

type Creature =
    { pos   :: Point
    , ctype :: CreatureType
    , stats :: Stats
    , speed :: Number
    , time  :: Number
    }

data Item = Loot { value :: Number, weight :: Number }
		  | Weapon { dmg :: Number, attackBonus :: Number, weight :: Number }
