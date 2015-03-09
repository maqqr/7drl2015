module Level where

import Data.Char
import Data.String (String)
import qualified Data.String as S
import Data.Array ((..), (!!), updateAt, map, length)
import qualified Data.Array.Unsafe as U
import Data.Maybe
import GameData
import Utils

data Tile = Air
          | Ground     | Grass      | Wall     | SWall
          | DoorLocked | DoorClosed | DoorOpen
          | BgCave     | BgHouse
          | Bush       | Stairs

data Level = Level
    { width  :: Number
    , height :: Number
    , tiles  :: [Tile]
    }

isTileSolid :: Tile -> Boolean
isTileSolid Ground     = true
isTileSolid Grass      = true
isTileSolid Wall       = true
isTileSolid SWall      = true
isTileSolid DoorLocked = true
isTileSolid DoorClosed = true
isTileSolid _          = false

isTileTransparent :: Tile -> Boolean
isTileTransparent Ground     = false
isTileTransparent Grass      = false
isTileTransparent Wall       = false
isTileTransparent SWall      = false
isTileTransparent DoorLocked = false
isTileTransparent DoorClosed = false
isTileTransparent Bush       = false
isTileTransparent _          = true

isTileClimbable :: Tile -> Boolean
isTileClimbable Stairs = true
isTileClimbable _      = false

newLevel :: Number -> Number -> Level
newLevel w h = Level { width: w, height: h, tiles: replicate (w * h) Air }

index :: Level -> Number -> Number -> Number
index (Level level) x y = x + y * level.width

getTile :: Level -> Point -> Maybe Tile
getTile l@(Level level) p | p.x > 0 && p.y > 0 && p.x < level.width && p.y < level.height =
    level.tiles !! index l p.x p.y
getTile l@(Level level) p | p.y >= level.height = Just Ground
getTile l@(Level level) p | otherwise = Just Air

setTile :: Level -> Point -> Tile -> Level
setTile l@(Level level) p newTile =
    Level $ level { tiles = updateAt (index l p.x p.y) newTile level.tiles }

levelPoints :: Level -> [Point]
levelPoints (Level level) = do
    x' <- 0 .. (level.width - 1)
    y' <- 0 .. (level.height - 1)
    return {x: x', y: y'}

stringToLevel :: [String] -> Level
stringToLevel strs = Level $ { width: S.length (U.head strs), height: length strs, tiles: makeTiles strs }
    where
        makeTiles :: [String] -> [Tile]
        makeTiles (x:xs) = map charToTile (S.toCharArray x) ++ makeTiles xs
        makeTiles _      = []

        charToTile :: Char -> Tile
        charToTile c | c == makeChar "." = Air
        charToTile c | c == makeChar "#" = Ground
        charToTile c | c == makeChar "G" = Grass
        charToTile c | c == makeChar "S" = Wall
        charToTile c | c == makeChar "W" = SWall
        charToTile c | c == makeChar "*" = DoorLocked
        charToTile c | c == makeChar "+" = DoorClosed
        charToTile c | c == makeChar "|" = DoorOpen
        charToTile c | c == makeChar "c" = BgCave
        charToTile c | c == makeChar "C" = BgHouse
        charToTile c | c == makeChar "B" = Bush
        charToTile c | c == makeChar "<" = Stairs
        charToTile _ = Air


emptyLevel :: [String]
emptyLevel =
    ["................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"]

testLevel :: [String]
testLevel =
    ["................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"................................................................................"
    ,"..............................................................####....#######..."
    ,"..............................................................................#."
    ,"............................................................................#..."
    ,"..............................................................................#."
    ,"............................................................................#..."
    ,"..............................................................................#."
    ,"............................................................................#..."
    ,"..............................................................................#."
    ,"..............................................#..cc#..cc#..#..c###...ccc#######."
    ,".............................................#ccccccccccccc##cccccccc##ccccccccc"
    ,"............................................#<cc#ccccccc####cccccccccccccccccccc"
    ,".........................................#.#c<cc#ccccccccccccccccccccccccccccccc"
    ,"........................................#cccc<c##ccccccccccccccccccccccccccccccc"
    ,".......................................#ccccc<cc##ccccc##ccccccccccccccccccccccc"
    ,"#..........B.............SSSWSSSS.....#ccccc#<cc#ccccccccccccccccccc#cccc#cccc##"
    ,"#...GGGGGGGGGG...........SCCCCCCS....##cccccc<ccccccc#cccccccccccccccccccccccccc"
    ,"#cc############cccc#.....SCCCSCCS.#.ccccccccc<cccccccccccBcccccccccccccccccccccc"
    ,"#cc############cccc###...|CCC*CC|.ccccccccccc<cccccccccccBBccccccccccccccccccccc"
    ,"cc#############cccc###GGG####+###cc#####cccGGGGGGGccccccGGGccccccccccccccccccccc"
    ,"cccccccccccccccccccccccccccccc####################GGGGGG######cccccccccccccccccc"
    ,"################################################################################"]
