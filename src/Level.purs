module Level where

import Data.Array ((..), (!!), updateAt)
import Data.Maybe
import GameData
import Utils

data Tile = Air | Ground

data Level = Level
    { width :: Number
    , height :: Number
    , tiles :: [Tile]
    }

newLevel :: Number -> Number -> Level
newLevel w h = Level { width: w, height: h, tiles: replicate (w * h) Air }

index :: Level -> Number -> Number -> Number
index (Level level) x y = x + y * level.width

getTile :: Level -> Point -> Maybe Tile
getTile l@(Level level) p = level.tiles !! index l p.x p.y

setTile :: Level -> Point -> Tile -> Level
setTile l@(Level level) p newTile =
    Level $ level { tiles = updateAt (index l p.x p.y) newTile level.tiles }

levelPoints :: Level -> [Point]
levelPoints (Level level) = do
    x' <- 0 .. (level.width - 1)
    y' <- 0 .. (level.height - 1)
    return {x: x', y: y'}
