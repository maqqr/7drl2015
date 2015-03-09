module Line where

import Data.Array
import Data.Maybe
import Data.Foreign
import GameData
import Level

lineOfSight :: Level -> Point -> Point -> Boolean
lineOfSight level start end = checkLine <<< drop 1 $ bresenham start.x start.y end.x end.y
    where
        isBlocked = not <<< isTileTransparent <<< fromMaybe Air <<< getTile level

        checkLine :: [Point] -> Boolean
        checkLine [] = true
        checkLine (_:[]) = true
        checkLine (p:oints) | isBlocked p = false
        checkLine (p:oints) | otherwise   = checkLine oints

foreign import bresenham
    """
    function bresenham(x0) {
        return function (y0) {
            return function (x1) {
                return function (y1) {
                    return bresenhamjs(x0, y0, x1, y1);
                }
            }
        }
    }
    """ :: Number -> Number -> Number -> Number -> [Point]
