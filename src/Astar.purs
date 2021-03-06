module Astar where

import Data.Maybe
import Data.Array
import Data.Foreign
import GameData

findPath :: Pathfinder -> Point -> Point -> [Point]
findPath pfind start end = drop 1 $ astar_js pfind start.x start.y end.x end.y

foreign import data Pathfinder :: *

foreign import makePathfinder
    """
    function makePathfinder(grid) {
        var easystar = new EasyStar.js();
        easystar.setGrid(grid);
        easystar.setAcceptableTiles([1]);
        easystar.enableDiagonals();
        if (grid.length === 0) {
            easystar.w = 0;
            easystar.h = 0;
        }
        else {
            easystar.w = grid[0].length;
            easystar.h = grid.length;
        }
        return easystar;
    }
    """ :: [[Number]] -> Pathfinder

foreign import astar_js
    """
    function astar_js(easystar) {
        return function (x0) {
            return function (y0) {
                return function (x1) {
                    return function (y1) {
                        if (!(x0 > 0 && x0 < easystar.w && y0 > 0 && y0 < easystar.h
                              && x1 > 0 && x1 < easystar.w && y1 > 0 && y1 < easystar.h)) {
                            return [];
                        }
                        var done = undefined;
                        easystar.findPath(x0, y0, x1, y1, function(path) {
                            done = path;
                        });
                        while (done === undefined) {
                            easystar.calculate();
                        }
                        if (done === null) {
                            done = [];
                        }
                        return done;
                    }
                }
            }
        }
    }
    """ :: Pathfinder -> Number -> Number -> Number -> Number -> [Point]
