module Level where

import Data.Char
import Data.String (String)
import qualified Data.String as S
import Data.Array ((..), (!!), updateAt, map, length)
import qualified Data.Array.Unsafe as U
import Data.Maybe
import GameData
import Utils

data Tile = Air | Ground | Wall | DoorLocked | DoorClosed | DoorOpen

data Level = Level
    { width  :: Number
    , height :: Number
    , tiles  :: [Tile]
    }

isTileSolid :: Tile -> Boolean
isTileSolid Air        = false
isTileSolid Ground     = true
isTileSolid Wall       = true
isTileSolid DoorLocked = true
isTileSolid DoorClosed = true
isTileSolid DoorOpen   = false
isTileSolid _          = false

isTileTransparent :: Tile -> Boolean
isTileTransparent Air        = true
isTileTransparent Ground     = false
isTileTransparent Wall       = false
isTileTransparent DoorLocked = false
isTileTransparent DoorClosed = false
isTileTransparent DoorOpen   = true
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
        charToTile c | c == makeChar "S" = Wall
        charToTile c | c == makeChar "*" = DoorLocked
        charToTile c | c == makeChar "+" = DoorClosed
        charToTile c | c == makeChar "|" = DoorOpen
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
    ,"#........................SSSSSSSS.....#.....#...#...................#....#....##"
    ,"#...##########...........S......S....##..............#.........................."
    ,"#..############....#.....S...S..S.#............................................."
    ,"#..############....###...|...*..|..............................................."
    ,"..#############....##########+###..#####...#######......###....................."
    ,"..............................################################.................."
    ,"################################################################################"]
