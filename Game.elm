import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space, isDown)

type CannonNum = Float
type RowNum = Float
type KeyDown = Bool
type CreatePlatform = Bool
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

newPlatform = { dir=Left, pos=-halfGameW, len=100 }


-- INPUT

homerow = [65, 83, 68, 70, 74, 75, 76, 186] -- asdf jkl;

input : Signal ([KeyDown], [KeyDown])
input = let spaceList = combine (repeat numPlatformRows space)
            keysList = combine (map isDown homerow) in
        sampleOn (fps 30) (lift2 (,) spaceList keysList)

--main = lift asText (foldp step bulletState input)
main = let initBulletsState = repeat numCannons []
           initPlatformsState = repeat numPlatformRows [newPlatform]
           initState = { ps = initPlatformsState, bbs = initBulletsState }
       in lift displayBullets (foldp step initState input)

-- STEP

step : ([CreatePlatform], [KeyDown]) -> GameState -> GameState
step (createPs, keys) = bulletsMove2
                        >> maybeAddBullets keys
                        >> platformsMove2
                        >> maybeAddPlatforms createPs

-- BULLETS

bulletSpeed = 5

maybeAddBullets : [KeyDown] -> GameState -> GameState
maybeAddBullets keys gs = { gs | bbs <- map maybeAddBullet (zip keys gs.bbs) }

maybeAddBullet : (KeyDown, BulletColState) -> BulletColState
maybeAddBullet (p,bs) = if p then cannonH::bs else bs

bulletsMove2 : GameState -> GameState
bulletsMove2 gs = { gs | bbs <- map bulletsMove gs.bbs }

bulletsMove : BulletColState -> BulletColState
bulletsMove xs = case xs of
                  [] -> []
                  (b::bs) -> if b < gameH
                             then (b + bulletSpeed) :: bulletsMove bs
                             else bulletsMove bs

-- PLATFORMS

platformSpeed = 5

maybeAddPlatforms : [CreatePlatform] -> GameState -> GameState
maybeAddPlatforms createPs gs = { gs | ps <- map maybeAddPlatform (zip createPs gs.ps) }

maybeAddPlatform : (CreatePlatform, PlatformRowState) -> PlatformRowState
maybeAddPlatform (createP, ps) = if createP then newPlatform::ps else ps

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