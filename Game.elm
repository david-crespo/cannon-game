import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space, isDown)

type CannonNum = Float
type RowNum = Float
type KeyDown = Bool
data Dir = Left | Right

type BulletColState = [Float]
type BulletsState = [BulletColState]

type PlatformState = { dir:Dir, pos:Float, len:Float }
type PlatformRowState = [PlatformState]
type PlatformsState = [PlatformRowState]

type GameState = { ps  : PlatformsState
                 , bbs : BulletsState
                 }

numCannons = 8
cannonSpacing = 70

numPlatformRows = 3
platformSpacing = 100


-- INPUT

homerow = [65, 83, 68, 70, 74, 75, 76, 186] -- asdf jkl;

input : Signal [KeyDown]
input = sampleOn (fps 30) (combine (map isDown homerow))

--main = lift asText (foldp step bulletState input)
main = let initBulletsState = repeat numCannons []
           initPlatformsState = repeat numPlatformRows [{ dir=Left, pos=0, len=100 }]
           initState = { ps = initPlatformsState, bbs = initBulletsState }
       in lift displayBullets (foldp step initState input)

-- STEP

step : [KeyDown] -> GameState -> GameState
step keys = bulletsMove2 >> maybeAddBullet2 keys >> platformsMove2

bulletSpeed = 5
platformSpeed = 5

maybeAddBullet2 : [KeyDown] -> GameState -> GameState
maybeAddBullet2 keys gs = { gs | bbs <- map maybeAddBullet (zip keys gs.bbs)}

maybeAddBullet : (KeyDown, BulletColState) -> BulletColState
maybeAddBullet (p,bs) = if p then (cannonH :: bs) else bs

bulletsMove2 : GameState -> GameState
bulletsMove2 gs = { gs | bbs <- map bulletsMove gs.bbs }

bulletsMove : BulletColState -> BulletColState
bulletsMove xs = case xs of
                  [] -> []
                  (b::bs) -> if b < gameH
                             then (b + bulletSpeed) :: bulletsMove bs
                             else bulletsMove bs

platformsMove2 : GameState -> GameState
platformsMove2 gs = { gs | ps <- map platformsMove gs.ps }

platformsMove : PlatformRowState -> PlatformRowState
platformsMove xs = case xs of
                     [] -> []
                     (p::ps) -> if p.pos < gameW
                                then { p | pos <- p.pos + platformSpeed } :: platformsMove ps
                                else platformsMove ps

-- DISPLAY CONFIG

myGrey = rgba 240 240 240 1
cannonColor = blue
bulletColor = black
platformColor = green

(gameW,gameH) = ((numCannons + 1) * cannonSpacing,
                 (numPlatformRows + 1) * platformSpacing)
(halfGameW,halfGameH) = (gameW/2,gameH/2)

(bulletW, bulletH) = (2, 6)
(cannonW, cannonH) = (6, 14)
(halfCannonW, halfCannonH) = (3, 7)
platformH = 20

greyBackground = rect gameW gameH |> filled myGrey

cannonXOffset : CannonNum -> Float
cannonXOffset n = n * cannonSpacing - halfGameW

platformYOffset : RowNum -> Float
platformYOffset n = n * platformSpacing - halfGameH


-- DISPLAY

displayBullets : GameState -> Element
displayBullets { ps, bbs } = collage gameW gameH (
                           greyBackground
                           :: (map cannon [1..numCannons])
                           ++ (concatMap bullets (zip [1..numCannons] bbs))
                           ++ (concatMap platforms (zip [1..numPlatformRows] ps)))

bullets : (CannonNum, BulletColState) -> [Form]
bullets (n, bs) = map (bullet n) bs

bullet : CannonNum -> Float -> Form
bullet n pos = move (cannonXOffset n, halfGameH - pos)
                    (rect bulletW bulletH |> filled bulletColor)

cannon : CannonNum -> Form
cannon n = move (cannonXOffset n, halfGameH - halfCannonH)
                (rect cannonW cannonH |> filled cannonColor)

platforms : (RowNum, PlatformRowState) -> [Form]
platforms (n, ps) = map (platform n) ps

platform : RowNum -> PlatformState -> Form
platform n {dir,pos,len} = let dm = multiplier dir in
                           move (dm * pos, platformYOffset n)
                           (rect len platformH |> filled platformColor)

multiplier : Dir -> Float
multiplier d = case d of
                 Left -> -1
                 Right -> 1