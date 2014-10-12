import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space)

type CannonNum = Int

-- MARIO
input = let delta = lift (\t -> t/20) (fps 30)
        in sampleOn delta (lift2 (,) space delta)

--main : Signal Element
--main = lift asText input

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
main = lift displayBullets (foldp bulletsStep [0, 100, 200] input)

displayBullets : [BulletPos] -> Element
displayBullets bPositions = collage gameW gameH (
                                [greyBackground]
                                --++ (map cannon [1..8])
                                ++ (map bullet bPositions))

maybeAddBullet : Bool -> [BulletPos] -> [BulletPos]
maybeAddBullet p bs = if p then addBullet bs else bs

bullet : BulletPos -> Form
bullet pos = move (0, halfGameH - pos) (rect bulletW bulletH |> filled bulletColor)

type BulletPos = Float

bulletsState : [BulletPos]
bulletsState = []

addBullet : [BulletPos] -> [BulletPos]
addBullet = (::) 0

bulletSpeed = 5

bulletsStep : (Bool, Time) -> [BulletPos] -> [BulletPos]
bulletsStep (p, t) xs = case xs of
                      [] -> if p then [0] else []
                      (b::bs) -> let newBs = maybeAddBullet p bs in
                                if b < gameH
                                then (b + bulletSpeed) :: bulletsStep (False, t) newBs
                                else bulletsStep (False, t) newBs

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