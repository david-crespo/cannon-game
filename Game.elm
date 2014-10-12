import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space, isDown)

type CannonNum = Float
type BulletPos = Float
type KeyDown = Bool

numCannons = 8
cannonSpacing = 70


-- INPUT

homerow = [65, 83, 68, 70, 74, 75, 76, 186] -- asdf jkl;

input : Signal [KeyDown]
input = sampleOn (fps 30) (combine (map isDown homerow))

--main = lift asText (foldp step bulletState input)
main = let emptyBulletState = repeat numCannons []
       in lift displayBullets (foldp step emptyBulletState input)

-- STEP

step keys = bulletsMove2 >> maybeAddBullet2 keys

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


-- DISPLAY CONFIG

myGrey = rgba 240 240 240 1
cannonColor = blue
bulletColor = black
(gameW,gameH) = ((numCannons + 1) * cannonSpacing, 400)
(halfGameW,halfGameH) = (gameW/2,200)
(bulletW, bulletH) = (2, 6)
(cannonW, cannonH) = (6, 14)
(halfCannonW, halfCannonH) = (3, 7)

greyBackground = rect gameW gameH |> filled myGrey

cannonXOffset : CannonNum -> Float
cannonXOffset n = n * cannonSpacing - halfGameW


-- DISPLAY

displayBullets : [[BulletPos]] -> Element
displayBullets bbs = collage gameW gameH (
                         greyBackground
                         :: (map cannon [1..numCannons])
                         ++ (concatMap bullets (zip [1..numCannons] bbs)))

bullets : (CannonNum, [BulletPos]) -> [Form]
bullets (n, bs) = map (bullet n) bs

bullet : CannonNum -> BulletPos -> Form
bullet n pos = move (cannonXOffset n, halfGameH - pos)
                    (rect bulletW bulletH |> filled bulletColor)

cannon : CannonNum -> Form
cannon n = move (cannonXOffset n, halfGameH - halfCannonH)
                (rect cannonW cannonH |> filled cannonColor)