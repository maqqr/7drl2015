module GameData where

import Data.String

type Point = { x :: Number, y :: Number }

type Stats = { hp :: Number }

defaultStats = { hp: 10 }

data CreatureType = Player { name :: String }
				  | Guard 
				  | Archer 
				  | Peasant

instance showCT :: Show CreatureType where
    show (Player { name = pname }) = pname
    show Guard                     = "guard"
    show Archer                    = "archer"
    show Peasant                   = "peasant"

instance eqCT :: Eq CreatureType where
    (==) a b = show a == show b
    (/=) a b = show a /= show b

type Creature = { pos :: Point, ctype :: CreatureType, stats :: Stats }
