import Dict (Dict, get, fromList)
import Keyboard as K

type CannonNum = Int

--main : Signal Element
--main = lift display K.lastPressed

-- map homerow keys to [0..7]
homerow : Dict K.KeyCode CannonNum
homerow = fromList (zip [65, 83, 68, 70, 74, 75, 76, 186] [1..8])

getCannonNum : K.KeyCode -> Maybe CannonNum
getCannonNum keyCode = get keyCode homerow


input = let delta = lift (\t -> t/20) (fps 25)
        in  sampleOn delta (lift2 (,) delta K.arrows)

display : Int -> Element
display keyCode =
    flow right
        [ plainText "The last key you pressed was: "
        , asText (getCannonNum keyCode)
        ]

-- Display

c = 240
backgroundColor = rgba c c c 1
cannonColor = blue
cannonSpacing = 70
(gameW,gameH) = (630,400)
(halfGameW,halfGameH) = (315,200)
(cannonW, cannonH) = (6, 20)
(halfCannonW, halfCannonH) = (3, 10)

background = rect gameW gameH |> filled backgroundColor

cannonXOffset : CannonNum -> Float
cannonXOffset n = toFloat (n * cannonSpacing - halfGameW)

cannon : CannonNum -> Form
cannon n = move (cannonXOffset n, halfGameH - halfCannonH) (rect cannonW cannonH |> filled cannonColor)

main : Element
main =
    collage gameW gameH (background :: (map cannon [1..8]))