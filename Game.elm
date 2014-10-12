import Dict (Dict, get, fromList)
import Keyboard (KeyCode, keysDown, arrows, space)

type CannonNum = Int

delta = inSeconds <~ fps 30
--input = sampleOn delta <| lift2 (,) (floatify <~ arrows) delta

--main : Signal Element
--main = sampleOn delta <| lift display keysDown

--main : Signal Element
--main = coloredBackground <~ colorSignal

coloredBackground   : Color -> Element
coloredBackground c = collage gameW gameH ([background c])

colorSignal : Signal Color
colorSignal = (\x -> if x then blue else backgroundColor) <~ space

-- map homerow keys to [0..7]
homerow : Dict KeyCode CannonNum
homerow = fromList (zip [65, 83, 68, 70, 74, 75, 76, 186] [1..8])

getCannonNum : KeyCode -> Maybe CannonNum
getCannonNum keyCode = get keyCode homerow

display : [Int] -> Element
display keyCodes =
    flow right
        [ plainText "The last key you pressed was: "
        , asText (map getCannonNum keyCodes)
        ]

main : Signal Element
main = displayBullets <~ foldp bulletsStep [0] bulletDelta

displayBullets : [BulletPos] -> Element
displayBullets bPositions = collage gameW gameH (
                                [greyBackground]
                                ++ (map bullet bPositions)
                                ++ (map cannon [1..8]))

bullet : BulletPos -> Form
bullet pos = move (0, halfGameH - pos) (rect bulletW bulletH |> filled bulletColor)

type BulletPos = Float

bulletDelta : Signal Time
bulletDelta = fps 30

bulletsState : [BulletPos]
bulletsState = []

addBullet : [BulletPos] -> [BulletPos]
addBullet = (::) 0

bulletSpeed = 5

bulletsStep : Time -> [BulletPos] -> [BulletPos]
bulletsStep _ xs = case xs of
                     [] -> []
                     (b::bs) -> if b < gameH
                                then (b + bulletSpeed) :: bs
                                else bs

-- Display

c = 240
backgroundColor = rgba c c c 1
cannonColor = blue
bulletColor = black
cannonSpacing = 70
(gameW,gameH) = (630,400)
(halfGameW,halfGameH) = (315,200)
(bulletW, bulletH) = (2, 6)
(cannonW, cannonH) = (6, 20)
(halfCannonW, halfCannonH) = (3, 10)

greyBackground = rect gameW gameH |> filled backgroundColor
background c = rect gameW gameH |> filled c

cannonXOffset : CannonNum -> Float
cannonXOffset n = toFloat (n * cannonSpacing - halfGameW)

cannon : CannonNum -> Form
cannon n = move (cannonXOffset n, halfGameH - halfCannonH)
                (rect cannonW cannonH |> filled cannonColor)

--main : Element
--main =
--    collage gameW gameH (greyBackground :: (map cannon [1..8]))