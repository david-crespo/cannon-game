import Graphics.Element (..)
import Text (..)
import Random (int, generate, initialSeed, Seed)
import Time (Time, every, second, fps, timestamp)
import Signal (Signal, (<~), (~), foldp, constant)

type alias GameState = { n : Int }

randInt : Int -> Int -> Seed -> GameState -> GameState
randInt a b seed gs = let (n', _) = generate (int a b) seed
                      in { gs | n <- n' }

currTime : Signal Time
currTime = fst <~ (timestamp (constant ()))

seed : Signal Time -> Signal Seed
seed t = initialSeed <~ (round <~ t)

main : Signal Element
main = asText <~ (.n <~ foldp (randInt 1 10) { n = 1 } (seed (every second)))

-- main : Signal Element
-- main = asText <~ (fst <~ (timestamp (constant 0)))
