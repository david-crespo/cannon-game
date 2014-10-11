import Dict (Dict, get, fromList)
import Keyboard as K

type CannonNum = Int

main : Signal Element
main = lift display K.lastPressed

-- map homerow keys to [0..7]
homerow : Dict K.KeyCode CannonNum
homerow = fromList (zip [65, 83, 68, 70, 74, 75, 76, 186] [0..7])

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



--homerow : Signal Int
--homerow =