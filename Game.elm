import Dict (Dict, get, fromList)
import Keyboard (KeyCode, space, isDown)
import Random (range)

type CannonNum = Float
type RowNum = Float
type KeyDown = Bool
type CreatePlatform = Bool
data Dir = Left | Right

type BulletState = { pos: Float }
type BulletColState = [BulletState]
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

-- MAIN

homerow = [83, 68, 70, 71, 72, 74, 75, 76] -- sdfg hjkl

pickFrac = (>) 2

every400 = (1, 10, every (400 * millisecond))
platformSignal (a,b,c) = lift pickFrac (range a b c)

input : Signal ([KeyDown], [KeyDown])
input = let spaceList = combine (map platformSignal (repeat numPlatformRows every400))
            keysList = combine (map isDown homerow) in
        sampleOn (fps 30) (lift2 (,) spaceList keysList)

--main = lift asText rand

main = let initBulletsState = repeat numCannons []
           initPlatformsState = repeat numPlatformRows []
           initState = { ps = initPlatformsState, bbs = initBulletsState }
       in lift display (foldp step initState input)


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
maybeAddBullet (p,bs) = if p then {pos=cannonH}::bs else bs

bulletsMove2 : GameState -> GameState
bulletsMove2 gs = { gs | bbs <- map bulletsMove gs.bbs }

bulletsMove : BulletColState -> BulletColState
bulletsMove xs = case xs of
                  [] -> []
                  (b::bs) -> if b.pos < gameH
                             then { b | pos <- b.pos + bulletSpeed } :: bulletsMove bs
                             else bulletsMove bs

-- PLATFORMS

platformSpeed = 5

maybeAddPlatforms : [CreatePlatform] -> GameState -> GameState
maybeAddPlatforms createPs gs = { gs | ps <- map maybeAddPlatform (zip createPs gs.ps) }

newPlatform = { dir=Left, pos=-halfGameW-50, len=100 }

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

instructions = move (0, -halfGameH + 10)
                    (toForm (plainText "Use SDFG HJKL to fire the cannons"))

-- DISPLAY

display : GameState -> Element
display { ps, bbs } = collage gameW gameH (
                        greyBackground
                        :: instructions
                        :: (map cannon [1..numCannons])
                        ++ (concatMap bullets (zip [1..numCannons] bbs))
                        ++ (concatMap platforms (zip [1..numPlatformRows] ps)))

bullets : (CannonNum, BulletColState) -> [Form]
bullets (n, bs) = map (bullet n) bs

bullet : CannonNum -> BulletState -> Form
bullet n b = move (cannonXOffset n, halfGameH - b.pos)
                    (rect bulletW bulletH |> filled bulletColor)

-- CANNON

cannon : CannonNum -> Form
cannon n = move (cannonXOffset n, halfGameH - halfCannonH)
                (rect cannonW cannonH |> filled cannonColor)

cannonXOffset : CannonNum -> Float
cannonXOffset n = n * cannonSpacing - halfGameW


-- PLATFORM

platforms : (RowNum, PlatformRowState) -> [Form]
platforms (n, ps) = map (platform n) ps

platform : RowNum -> PlatformState -> Form
platform n {dir,pos,len} = let dm = multiplier dir in
                           move (dm * pos, platformYOffset n)
                           (rect len platformH |> filled platformColor)

platformYOffset : RowNum -> Float
platformYOffset n = n * platformSpacing - halfGameH

multiplier : Dir -> Float
multiplier d = case d of
                 Left -> -1
                 Right -> 1