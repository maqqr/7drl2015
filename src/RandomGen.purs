module RandomGen where

import Prelude.Unsafe
import Data.Array

import GameState
import GameData
import Level
import Math

-- Generates random number.
generate :: GameState -> { n :: Number, game :: GameState }
generate (Game state) =
    let new = (a * state.seed + c) % m in { n: new, game: Game state { seed = new } }
    where
        a = 0x343FD
        c = 0x269EC
        m = pow 32 2

-- Generates random integer between min' and max'
randInt :: Number -> Number -> GameState -> { n :: Number, game :: GameState }
randInt min' max' g = let gg = generate g in  gg { n = min' + (gg.n % (max' - min')) }

-- Generates random point.
randomPoint :: GameState -> { p :: Point, game :: GameState }
randomPoint g@(Game { level = (Level level) }) =
    let x = randInt 0 level.width g
        y = randInt 0 level.height x.game
    in { p: {x: x.n, y: y.n}, game: y.game }

choice :: forall a. [a] -> GameState -> { i :: a, game :: GameState }
choice list g = let r = randInt 0 (length list) g
                in { i: unsafeIndex list r.n, game: r.game }
