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
allLevels = [{ name: "a small village"
             , plan: tutorialLevel
             , itemPos: [p 55 6, p 62 5, p 23 9]
             , lootPos: [p 40 9, p 48 9, p 64 9, p 66 9, p 21 9, p 22 9]
             , npcPos: [patrol (p 59 9)]
             , startPos: {x: 2, y: 7}}

            ,{ name: "a small castle"
             , plan: castleLevel
             , itemPos: [p 30 13, p 45 13, p 72 13, p 76 6, p 27 6]
             , lootPos: [p 24 10, p 24 13, p 39 16, p 69 13, p 70 13, p 71 13]
             , npcPos: [patrol (p 37 10), patrol (p 72 10), aIdle (p 76 3), aIdle (p 24 6)]
             , startPos: {x: 2, y: 15}}

            ,{ name: "a small town with clock tower"
             , plan: clockTower
             , itemPos: [p 31 14, p 24 9, p 29 2, p 43 14, p 56 17]
             , lootPos: [p 19 17, p 10 16, p 11 16, p 40 11, p 40 14, p 42 14, p 44 17]
             , npcPos: [aIdle (p 24 6), patrol (p 60 17), patrol (p 33 6)]
             , startPos: {x: 2, y: 15}}

            ,{ name: "a hidden pirate cove"
             , plan: pirateCove
             , itemPos: [p 34 14, p 25 14, p 30 11, p 35 11, p 78 13]
             , lootPos: [p 54 13, p 23 14, p 24 14, p 28 11, p 76 13, p 74 13]
             , npcPos: [patrol (p 77 13), patrol (p 46 13)]
             , startPos: {x: 3, y: 13}}]
    where
        p x y = {x: x, y: y}

        idler p' = { p: p', ctype: Guard, ai: Idle p' }
        patrol p' = { p: p', ctype: Guard, ai: Patrol p' }

        aIdle p' = { p: p', ctype: Archer, ai: Idle p' }


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
    ,"...............................%................SSSCCCCCS................%%%%..S"
    ,"......................####....%%%...............I.+CCCCCS.....SSSSSSS...%%I%...S"
    ,"................B..GG#####....%I%%....SSSS*S..SSSSSSS<SSS.....SCCCCCS.....I....S"
    ,"#####GG..B......GGG##ccc###....I......SCCC<S..SCCCSCC<CCS.....SCCCCCS.....I....S"
    ,"#######GGGG===GG#####ccccccB.B.I..GG..*CCC<*..*CCC+CC<CC*.....*CCCCC*...B.IB...S"
    ,"###########===#############GGGGGGG##GGSSSSSSGGSSSSSSSSSSSGGGGGGGGGGGGGGGGGGGGGGG"
    ,"###########==###################################################################"
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


clockTower :: [String]
clockTower =
    ["......................................................................."
    ,"......................................................................."
    ,"......................................................................."
    ,"............................SSS........................................"
    ,"...........................SSSSS......................................."
    ,"..........................SSSSSSS......................................"
    ,"..........................I.....I......................................"
    ,".........................SSSS*SSSS....................................."
    ,"..........................ICC<CCI.....................................S"
    ,"..........................SCC<CCS.....................................S"
    ,"..........................SSS+SSS.....................................S"
    ,"..........................SCC<CCS.....................................S"
    ,"..........................SCC<CCS......SSSSSSS........................S"
    ,"............%.............ICC<CCI......SCCCCCS........................S"
    ,"...........%%%............SCC<CCS......SCCCCC*..S.....................S"
    ,"...........%I%...SSSS+SS..SSS+SSS......SSSSSSSSSSSS..SSSSSSSSSS.......S"
    ,"####........I....SCCC<CS..SCC<CCS......SCCCCCCCCCCS..SCCCCCCCCS.......S"
    ,"####......B.I....*CCC<C*..*CC<CC*......*CCCCCCCCCC*..*CCCCCCCC*....B..S"
    ,"########GGGGGG###################################################GGGGG#"
    ,"#######################################################################"]

pirateCove :: [String]
pirateCove =
    ["................................................................................"
    ,"................................................................................"
    ,".................................%.............................................."
    ,"................................%%%............................................I"
    ,"................................%I%............................................S"
    ,".................................I.....B......................................SS"
    ,".................................I.B..GGGG.....................................S"
    ,"............................B.GGGGGGGG#########.............................I..S"
    ,"............................GG#####################........................SSSSS"
    ,".........................###############################....................I..S"
    ,".......................####ccccc#cccc#ccc################......................S"
    ,"......................#####ccccc*cccc*ccccc##########ccc.......................S"
    ,"......................#####c#############ccccccc####cccc........I....I.........S"
    ,".....................##ccc##c##############ccccccc+cccccc.......SSSSSSCCCCCCCCCS"
    ,"####................###ccc+ccccccccccccc+cc#########SSSSSS==========SSSSSSSSSSSS"
    ,"#######...........################################========================SSSSSS"
    ,"###########======#################=============================================S"
    ,"#############======================#############################################"
    ,"################################################################################"]

mines :: [String]
mines =
    ["...................................#############################################"
    ,"........................%........%%#############################################"
    ,"...........%%..........%%%......%%%*cccccccccccccccccccccccccccccccccccccccccc##"
    ,"...........%%%........%%I%.....%%%%##########################################<##"
    ,"...........%I%..........I%......%I%##############cccc#################CCCCC##<##"
    ,"....B.......I.......B..BI........I.+ccccccccccccIccccIccccccccccccccc*CCCCC##<##"
    ,"GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG#################cc#######c#################<##"
    ,"#################################################cc########c#################<##"
    ,"########################################cccccccccc#########c#################<##"
    ,"####CCCC##################CCCC#################c<##########c####cc###########<##"
    ,"####CCCC*cccccccccccccccc*CCCC+ccccccccccccccccc<ccccccccccccccccccccccc#####<##"
    ,"###########<###################################c<##############ccc###########<##"
    ,"###cccccccc<##################ccccc#############cc##############c###cc#######<##"
    ,"####cc#####<######################cc#############ccccccccc########ccccc######<##"
    ,"###cc######<#####CCCC############cc###############cc########c######c#########<##"
    ,"###ccccccccccccc*CCCC+cccccccccccccccccccccccccccccccccccccccccccccc#########<##"
    ,"######################################c######c#############cc################c##"
    ,"#######ccccccccccccccccccccccccc#####cc###cccccc#############################c##"
    ,"###############################c#############################################c##"
    ,"######c############cccccccccccccccc<<<<<cccccccccccccccccc*cccccccc*cccccccc*cc#"
    ,"#####ccc#########ccc################ccc#######################################<#"
    ,"####ccccc##########c################ccc#################ccccccc###############<#"
    ,"###IccCCcI#######cccccccccccc#######cccc###############cc#####cc##############<#"
    ,"##cIcCCcCIc########################cc#c###############cc#######cc#############<#"
    ,"#CCIccCCcIcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc<#"
    ,"################################################################################"]
