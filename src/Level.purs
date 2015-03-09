module Level where

import Data.Char
import Data.String (String)
import qualified Data.String as S
import Data.Array ((..), (!!), updateAt, map, length)
import qualified Data.Array.Unsafe as U
import Data.Maybe
import GameData
import Utils

data Tile = Air | Ground | Grass | Wall 
          | DoorLocked | DoorClosed | DoorOpen
          | BgCave     | BgHouse
          | Bush

data Level = Level
    { width  :: Number
    , height :: Number
    , tiles  :: [Tile]
    }

isTileSolid :: Tile -> Boolean
isTileSolid Air        = false
isTileSolid Ground     = true
isTileSolid Grass      = true
isTileSolid Wall       = true
isTileSolid DoorLocked = true
isTileSolid DoorClosed = true
isTileSolid DoorOpen   = false
isTileSolid BgCave     = false
isTileSolid BgHouse    = false
isTileSolid Bush       = false
isTileSolid _          = false

isTileTransparent :: Tile -> Boolean
isTileTransparent Air        = true
isTileTransparent Ground     = false
isTileTransparent Grass      = false
isTileTransparent Wall       = false
isTileTransparent DoorLocked = false
isTileTransparent DoorClosed = false
isTileTransparent DoorOpen   = true
isTileTransparent BgCave     = true
isTileTransparent BgHouse    = true
isTileTransparent Bush       = false
isTileTransparent _          = true

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
        charToTile c | c == makeChar "*" = DoorLocked
        charToTile c | c == makeChar "+" = DoorClosed
        charToTile c | c == makeChar "|" = DoorOpen
        charToTile c | c == makeChar "c" = BgCave
        charToTile c | c == makeChar "C" = BgHouse
        charToTile c | c == makeChar "B" = Bush
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
    ,"................................................................................"
    ,"..............................................#....#....#..#...###......#######."
    ,".............................................#.............##........##........."
    ,"............................................#...#.......####...................."
    ,".........................................#.#....#..............................."
    ,"........................................#......##..............................."
    ,".......................................#........##.....##......................."
    ,"#..........B.............SSSSSSSS.....#.....#...#...................#....#....##"
    ,"#...GGGGGGGGGG...........SCCCCCCS....##..............#.........................."
    ,"#cc############cccc#.....SCCCSCCS.#............................................."
    ,"#cc############cccc###...|CCC*CC|........................B......................"
    ,"cc#############cccc###GGG####+###..#####...GGGGGGG......GGG....................."
    ,"cccccccccccccccccccccccccccccc####################GGGGGG######.................."
    ,"################################################################################"]
