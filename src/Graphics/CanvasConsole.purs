module Graphics.CanvasConsole where

import Data.Foreign
import Control.Monad.Eff

foreign import data ConEff :: !

foreign import data Console :: *


foreign import withConsole
    """
    function withConsole (w) {
        return function (h) {
            return function (initialState) {
                return function (func) {
                    return function (keyFunc) {
                        return function () {
                            return new Console(w, h, initialState, func, keyFunc);
                        }
                    }
                }
            }
        }
    }
    """ :: forall eff s.
              Number -- Width
           -> Number -- Height
           -> s      -- Initial state
           -> (Console -> s -> Eff (con :: ConEff | eff) Unit) -- Initialization callback
           -> (Console -> s -> Number -> Eff (con :: ConEff | eff) s)    -- Keypress callback
           -> Eff (con :: ConEff | eff) Unit


foreign import clear
    """
    function clear(con) {
        return function () {
            con.clear();
        }
    }
    """ :: forall eff. Console -> Eff (con :: ConEff | eff) Unit


foreign import drawChar
    """
    function drawChar(con) {
        return function (ch) {
            return function(col) {
                return function (x) {
                    return function (y) {
                        return function() {
                            con.drawChar(ch, col, x, y);
                        }
                    }
                }
            }
        }
    }
    """ :: forall eff.
              Console
           -> String  -- Character
           -> String  -- Color
           -> Number  -- X
           -> Number  -- Y
           -> Eff (con :: ConEff | eff) Unit


foreign import drawString
    """
    function drawString(con) {
        return function (txt) {
            return function(col) {
                return function (x) {
                    return function (y) {
                        return function() {
                            con.drawString(txt, col, x, y);
                        }
                    }
                }
            }
        }
    }
    """ :: forall eff.
              Console
           -> String  -- Character
           -> String  -- Color
           -> Number  -- X
           -> Number  -- Y
           -> Eff (con :: ConEff | eff) Unit