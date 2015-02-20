import Graphics.Element (..)
import Text (..)
import Random (int, generate, initialSeed, Seed)
import Time (Time, every, second, fps, timestamp)
import Signal

type alias GameState = { seed : Seed }
type alias CountState = Int

-- getTime : Time
-- getTime = fst <| timestamp <| constant 5

randInt : Int -> Int -> GameState -> (Int, GameState)
randInt a b gs = let (i, seed') = generate (int a b) gs.seed
                 in (i, {gs | seed <- seed' } )

inc : Time -> Int -> Int
inc _ = (+) 1


-- main : Signal Element
-- main = Signal.map asText (Signal.foldp inc 1 (every second))

main : Signal Element
main = Signal.map asText (every second)
