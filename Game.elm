import Dict (Dict, get, fromList)
import Keyboard as K

type CannonNum = Int

main : Signal Element
main = lift display K.lastPressed

-- keycodes for [a, s, d, f, j ,k, l, ;]
homerow : Dict K.KeyCode CannonNum
homerow = fromList [
    (65, 0),
    (83, 1),
    (68, 2),
    (70, 3),
    (74, 4),
    (75, 5),
    (76, 6),
    (186, 7)]

--keymap : Dict Int Int
--keymap : fromList [ (65, 1) ]

input = let delta = lift (\t -> t/20) (fps 25)
        in  sampleOn delta (lift2 (,) delta K.arrows)

display : Int -> Element
display keyCode =
    flow right
        [ plainText "The last key you pressed was: "
        , asText (get keyCode homerow)
        ]



--homerow : Signal Int
--homerow =