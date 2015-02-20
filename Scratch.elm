import Graphics.Element (..)
import Text (..)
import Random (int, generate, initialSeed, Seed)
import Time (Time, every, second, fps, timestamp)
import Signal (Signal, constant)

type alias GameState = { seed : Seed }

-- getTime : Time
-- getTime = fst <| timestamp <| constant 5

randInt : Int -> Int -> GameState -> (Int, GameState)
randInt a b gs = let (i, seed') = generate (int a b) gs.seed
                 in (i, {gs | seed <- seed' } )

main : Element
main = asText <| fst <| randInt 1 10 { seed = initialSeed 5 }
