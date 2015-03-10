module Astar where

import Data.Maybe
import Data.Array
import Data.Foreign
import GameData

findPath :: Graph -> Point -> Point -> [Point]
findPath graph start end = astar_js graph start.x start.y end.x end.y

foreign import data Graph :: *

foreign import makeGraph
    """
    function makeGraph(arr) {
        return new Graph(arr);
    }
    """ :: [[Number]] -> Graph

foreign import astar_js
    """
    function astar_js(graph) {
        return function (x0) {
            return function (y0) {
                return function (x1) {
                    return function (y1) {
                        var start = graph.grid[y0][x0];
                        var end   = graph.grid[y1][x1];
                        return astar.search(graph, start, end);
                    }
                }
            }
        }
    }
    """ :: Graph -> Number -> Number -> Number -> Number -> [Point]
