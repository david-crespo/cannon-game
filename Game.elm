import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space, isDown)

type CannonNum = Int

-- MARIO
input = sampleOn (fps 30) (combine (map isDown homerowCodes))

homerowCodes = [65, 83, 68, 70, 74, 75, 76, 186]

homerow : Dict KeyCode CannonNum
homerow = fromList (zip homerowCodes [1..8])

getCannonNum : KeyCode -> Maybe CannonNum
getCannonNum keyCode = get keyCode homerow

display : [Bool] -> Element
display keyCodes =
    flow right
        [ plainText "The last key you pressed was: "
        , asText keyCodes
        ]

--main = lift display input

main : Signal Element
main = lift displayBullets (foldp step [[]] input)

displayBullets : [[BulletPos]] -> Element
displayBullets bbs = collage gameW gameH (
                         greyBackground
                         :: (map cannon [1..8])
                         ++ (concatMap bullets (zip [1..8] bbs)))

bullets : (CannonNum, [BulletPos]) -> [Form]
bullets (n, bs) = map (bullet n) bs

bullet : CannonNum -> BulletPos -> Form
bullet n pos = move (cannonXOffset n, halfGameH - pos)
                    (rect bulletW bulletH |> filled bulletColor)

type BulletPos = Float
type KeyDown = Bool

step keys =  bulletsMove2 >> maybeAddBullet2 keys

bulletSpeed = 5

maybeAddBullet2 : [KeyDown] -> [[BulletPos]] -> [[BulletPos]]
maybeAddBullet2 ps bbs = map maybeAddBullet (zip ps bbs)

maybeAddBullet : (KeyDown, [BulletPos]) -> [BulletPos]
maybeAddBullet (p,bs) = if p then (cannonH :: bs) else bs

bulletsMove2 : [[BulletPos]] -> [[BulletPos]]
bulletsMove2 = map bulletsMove

bulletsMove : [BulletPos] -> [BulletPos]
bulletsMove xs = case xs of
                  [] -> []
                  (b::bs) -> if b < gameH
                             then (b + bulletSpeed) :: bulletsMove bs
                             else bulletsMove bs

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