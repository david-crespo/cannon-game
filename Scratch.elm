import Graphics.Element (..)
import Text (..)
import Random (int, generate, initialSeed, Seed)
import Time (Time, every, second, fps, timestamp)
import Signal (Signal, (<~), foldp)

type alias GameState = { n : Int
                       , seed : Seed }

randInt : Time -> GameState -> GameState
randInt _ gs = let (n', seed') = generate (int 1 10) gs.seed
               in { gs | seed <- seed'
                       , n <- n' }

initState = { n=1 , seed = initialSeed 0 }

main : Signal Element
main = asText <~ (.n <~ foldp randInt initState (every second))
