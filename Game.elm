import Color exposing (..)
import Dict exposing (Dict, get, fromList)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard exposing (KeyCode, space, isDown)
import List exposing ((::), concatMap, foldr, map, map2, repeat)
import Random as R
import Signal as S
import Text
import Time exposing (Time, every, millisecond, fps)

type alias CannonNum = Float
type alias RowNum = Float
type alias KeyDown = Bool
type alias CreatePlatform = Bool
type       Dir = Left | Right

type alias BulletState = { pos: Float }
type alias BulletColState = List BulletState
type alias BulletsState = List BulletColState

type alias PlatformState = { dir:Dir, pos:Float, len:Float }
type alias PlatformRowState = List PlatformState
type alias PlatformsState = List PlatformRowState

type alias KeysAndPs = (List KeyDown, List CreatePlatform)

type alias GameState = { ps   : PlatformsState
                       , bbs  : BulletsState
                       }

numCannons = 8
cannonSpacing = 70

numPlatformRows = 3
platformSpacing = 100

sZip = S.map2 (,)
zip = map2 (,)

-- MAIN

homerow = [83, 68, 70, 71, 72, 74, 75, 76] -- sdfg hjkl

pickFrac = (>) 2

g : R.Generator Int
g = R.int 1 10

every400 = every (400 * millisecond)

folder : Time -> (Int, R.Seed) -> (Int, R.Seed)
folder t (i, s) = R.generate g s

initialSeeds = map R.initialSeed [453, 69043, 2383]

randBools iSeed =
    let signal = S.foldp folder (0, iSeed) every400
    in S.map (\(i,s) -> (i < 2)) signal

{-| Combine a list of signals into a signal of lists. -}
combine : List (Signal a) -> Signal (List a)
combine = foldr (S.map2 (::)) (S.constant [])

input : Signal KeysAndPs
input = let keys = combine (map isDown homerow)
            ps = combine (map randBools initialSeeds)
        in S.sampleOn (fps 30) (S.map2 (,) keys ps)

--main = map asText rand

main = let initBulletsState = repeat numCannons []
           initPlatformsState = repeat numPlatformRows []
           initState = { ps = initPlatformsState, bbs = initBulletsState }
       in S.map display (S.foldp step initState input)


-- STEP

step : KeysAndPs -> GameState -> GameState
step (keys, createPs) = bulletsMove2
                        >> maybeAddBullets keys
                        >> platformsMove2
                        >> maybeAddPlatforms createPs

-- BULLETS

bulletSpeed = 5

maybeAddBullets : List KeyDown -> GameState -> GameState
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

maybeAddPlatforms : List CreatePlatform -> GameState -> GameState
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
                    (toForm (centered (Text.fromString "Use SDFG HJKL to fire the cannons")))

-- DISPLAY

display : GameState -> Element
display { ps, bbs } = collage gameW gameH (
                        greyBackground
                        :: instructions
                        :: (map cannon [1..numCannons])
                        ++ (concatMap bullets (zip [1..numCannons] bbs))
                        ++ (concatMap platforms (zip [1..numPlatformRows] ps)))

bullets : (CannonNum, BulletColState) -> List Form
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

platforms : (RowNum, PlatformRowState) -> List Form
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
