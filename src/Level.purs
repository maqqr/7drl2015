module Level where

import Data.Char
import Data.String (String)
import qualified Data.String as S
import Data.Array ((..), (!!), updateAt, map, length)
import qualified Data.Array.Unsafe as U
import Data.Maybe
import GameData
import Utils
import Astar

data Tile = Air        | Water      | BrickWall
          | Ground     | Grass      | Wall      | SWall
          | DoorLocked | DoorClosed | DoorOpen
          | BgCave     | BgHouse
          | Bush       | Stairs
          | Trunk      | Leaves

data Level = Level
    { width  :: Number
    , height :: Number
    , tiles  :: [Tile]
    }

isTileSolid :: Tile -> Boolean
isTileSolid Ground     = true
isTileSolid Grass      = true
isTileSolid Wall       = true
isTileSolid BrickWall  = true
isTileSolid SWall      = true
isTileSolid DoorLocked = true
isTileSolid DoorClosed = true
isTileSolid _          = false

isTileTransparent :: Tile -> Boolean
isTileTransparent Ground     = false
isTileTransparent Grass      = false
isTileTransparent Wall       = false
isTileTransparent BrickWall  = false
isTileTransparent SWall      = false
isTileTransparent DoorLocked = false
isTileTransparent DoorClosed = false
isTileTransparent Bush       = false
isTileTransparent Water      = false
isTileTransparent Leaves     = false
isTileTransparent _          = true

isTileClimbable :: Tile -> Boolean
isTileClimbable Stairs     = true
isTileClimbable Water      = true
isTileClimbable DoorOpen   = true
isTileClimbable DoorClosed = true
isTileClimbable DoorLocked = true
isTileClimbable Leaves     = true
isTileClimbable _          = false

-- Calculates tile weights for pathfinding.
levelWeights :: Level -> [[Number]]
levelWeights l@(Level level) = do
    y <- 0 .. (level.height - 1)
    [makeRow y 0]
    where
        makeRow :: Number -> Number -> [Number]
        makeRow i x | x < level.width = tileWeight {x: x, y: i} : makeRow i (x + 1)
        makeRow i x | otherwise       = []

        tileWeight :: Point -> Number
        tileWeight p | canWalkOn p || canClimbOn p = 1
        tileWeight p | otherwise                   = 0

        getTile' :: Point -> Tile
        getTile' = fromMaybe Ground <<< getTile l

        canWalkOn :: Point -> Boolean
        canWalkOn p = blocked (p .+. {x:0, y:1}) && not (blocked p)

        canClimbOn :: Point -> Boolean
        canClimbOn = isTileClimbable <<< getTile'

        blocked :: Point -> Boolean
        blocked = blocksNpc <<< getTile'

        blocksNpc :: Tile -> Boolean
        blocksNpc DoorOpen   = false
        blocksNpc DoorClosed = false
        blocksNpc DoorLocked = false
        blocksNpc t = isTileSolid t

newLevel :: Number -> Number -> Level
newLevel w h = Level { width: w, height: h, tiles: replicate (w * h) Air }

emptyLevel :: Level
emptyLevel = Level { width: 0, height: 0, tiles: [] }

index :: Level -> Number -> Number -> Number
index (Level level) x y = x + y * level.width

getTile :: Level -> Point -> Maybe Tile
getTile l@(Level level) p | p.x >= 0 && p.y > 0 && p.x < level.width && p.y < level.height =
    level.tiles !! index l p.x p.y
getTile l@(Level level) p | p.y >= level.height = Just Ground
getTile l@(Level level) p | otherwise = Just Air

setTile :: Level -> Point -> Tile -> Level
setTile l@(Level level) p newTile =
    Level $ level { tiles = updateAt (index l p.x p.y) newTile level.tiles }

isValidMove :: Level -> Point -> Boolean
isValidMove level = not <<< isTileSolid <<< fromMaybe Air <<< getTile level

isTransparent :: Level -> Point -> Boolean
isTransparent level = isTileTransparent <<< fromMaybe Air <<< getTile level

isClimbable :: Level -> Point -> Boolean
isClimbable level = isTileClimbable <<< fromMaybe Air <<< getTile level

inFreeFall :: forall r. Level -> { pos :: Point, vel :: Point | r } -> Boolean
inFreeFall level c = isValidMove level (c.pos .+. {x:0, y: 1}) || c.vel.y < 0

-- Returns all points inside level.
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
        charToTile c | c == makeChar "$" = BrickWall
        charToTile c | c == makeChar "*" = DoorLocked
        charToTile c | c == makeChar "+" = DoorClosed
        charToTile c | c == makeChar "|" = DoorOpen
        charToTile c | c == makeChar "c" = BgCave
        charToTile c | c == makeChar "C" = BgHouse
        charToTile c | c == makeChar "B" = Bush
        charToTile c | c == makeChar "=" = Water
        charToTile c | c == makeChar "<" = Stairs
        charToTile c | c == makeChar "I" = Trunk
        charToTile c | c == makeChar "%" = Leaves
        charToTile _ = Air


type LevelDefinition =
    { name      :: String
    , plan      :: [String]
    , itemPos   :: [Point]
    , lootPos   :: [Point]
    , npcPos    :: [{ p :: Point, ctype :: CreatureType, ai :: AIState }]
    , startPos  :: Point
    }

allLevels :: [LevelDefinition]
allLevels = [{ name: "the tutorial village"
             , plan: tutorialLevel
             , itemPos: [p 55 6, p 62 5, p 23 9]
             , lootPos: [p 40 9, p 48 9, p 64 9, p 66 9, p 21 9, p 22 9]
             , npcPos: [patrol (p 59 9)]
             , startPos: {x: 2, y: 7}}]
    where
        p x y = {x: x, y: y}

        idler p' = { p: p', ctype: Guard, ai: Idle p' }
        patrol p' = { p: p', ctype: Guard, ai: Patrol p' }

emptyLevel' :: [String]
emptyLevel' =
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

tutorialLevel :: [String]
tutorialLevel =
    ["................................................................................"
    ,"................................................................................"
    ,"...............................................................................S"
    ,"...............................................................................S"
    ,"..................................................SSSSSSS.................%%.. S"
    ,"...............................%..................SCCCCCS................%%%%..S"
    ,"......................####....%%%.............I...+CCCCCS.....SSSSSSS....%I%...S"
    ,"................B..GG#####....%I%%....SSS*SS..SSSSSSS<SSS.....SCCCCCS.....I....S"
    ,"#####GG..B......GGG##ccc###....I......SCC<CS..SCCCSCC<CCS.....SCCCCCS.....I....S"
    ,"#######GGGG===GG#####ccccccB.B.I......*CC<C*..*CCC+CC<CC*.....*CCCCC*...B.IB...S"
    ,"###########===#############GGGGGGGGGGGSSSSSSGGSSSSSSSSSSSGGGGGGGGGGGGGGGGGGGGGGG"
    ,"###########==###################################################################"
    ,"################################################################################"]

testLevel :: [String]
testLevel =
    ["................................................................................"
    ,"................................................................................"
    ,"......................SSS*SSSSSSSSSS............................................"
    ,".....................CCCCCCCCCCCCCCS............................................"
    ,"...................SSSSSS*SSSSSSSSSS............................................"
    ,".................CCCCCCCCCCCCCCCCCC*....C....CC.....CCC...C....................."
    ,".................SSSSSSSS*SSSSSSSSSSS...S....SSC....SSS...S...####....#######..."
    ,"..............................................SS..............................#."
    ,"............................................................................#..."
    ,"..............................................................................#."
    ,"............................................................................#..."
    ,"..............................................................................#."
    ,".........................S..................................................#..."
    ,"..............................................................................#."
    ,"..............................................#..cc#..cc#..#..c###...ccc#######."
    ,".............................................#ccccccccccccc##cccccccc##ccccccccc"
    ,".............%..............................#<cc#ccccccc####cccccccccccccccccccc"
    ,"............%%%%..%%%....................#.#c<cc#ccccccccccccccccccccccccccccccc"
    ,"............%I%...%%%...................#cccc<c##ccccccccccccccccccccccccccccccc"
    ,".............I....%I%..................#ccccc<cc##ccccc##ccccccccccccccccccccccc"
    ,"#..........B.I.....I.....SSSWSSSS.....#ccccc#<cc#ccccccccccccccccccc#cccc#cccc##"
    ,"#...GGGGGGGGGG.....I.....SCCCCCCS....##cccccc<ccccccc#cccccccccccccccccccccccccc"
    ,"#==############====#.....SCCCSCCS.#.ccccccccc<cccccccccccBcccccccccccccccccccccc"
    ,"#==############====###...|CCC*CC|.ccccccccccc<cccccccccccBBccccccccccccccccccccc"
    ,"==#############====###GGG####*###cc#####cccGGGGGGGccccccGGGccccccccccccccccccccc"
    ,"==============================####################GGGGGG######cccccccccccccccccc"
    ,"################################################################################"]

castleLevel :: [String]
castleLevel =
    ["...............................................................................$"
    ,"...............................................................................$"
    ,"...............................................................................$"
    ,"..........................................................................$....$"
    ,"..........................................................................$$$$*$"
    ,"...........................................................................$CC<$"
    ,"......................$.....$..............................................$CC<$"
    ,"......................$$$$+$$..............................................$$$+$"
    ,".......................$CC<$.....................$$$$$$$$$$$$$$$...........$CC<$"
    ,".......................$CC<$.....................$CCCCCCC$CCCCC$...........$CC<$"
    ,".......................$CC<*.....................*CCCCCCC+CCCCC*...........*CC<$"
    ,".......................$$$+$$$$$$*$$$$$$$$$$$$$$$$$$+$$$$$$$$$$$$$$$$$$$$$$$$$$$"
    ,"...................%...$CC<$CCCCC<CCCC$CCCCCCCCCC$CC<CCCC$CCCCCCCCCC$CCCCCC$CCC$"
    ,"..................%%%%.$CC<$CCCCC<CCCC+CCCCCCCCCC+CC<CCCC$CCCCCCCCCC$CCCCCC*CCC$"
    ,"..................%I%..$$$+$$$$$$$$$$$$$$$$$$$$$$$$$+$$$$$CCCCCCCCCC$$$$$$$$$$C$"
    ,"#..................I...$CC<$CCCCCCC$CCCCCC$CCCCCCCCC<CCCC$CCCCICICCC$CCCCCCCCC$$"
    ,"##G.B.......B......I...*CC<+CCCCCCC+CCCCCC*CCCCCCCCC<CCCC*CCCC$=$CCC*CCCCCCCC$$$"
    ,"###GG====GGGGGGGGGGGGGG$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$=$$$$$$$$$$$$$$$$"
    ,"######=======##################=======############ccccccc######=################"
    ,"#######================########=#####=#####=========#c#cc######=################"
    ,"#########====#########==========#####=======#########c##========################"
    ,"################################################################################"]
