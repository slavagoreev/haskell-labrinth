{-# LANGUAGE OverloadedStrings #-}
-- | Levels for Escape the Room.
module EscapeTheRoom.Levels where

import qualified CodeWorld
import           Data.Text (Text)

-- | Tiles used in Escape the Room game.
data Tile
  -- standard tiles
  = Wall              -- ^ An unpassable wall.
  | Floor             -- ^ Floor to walk on.
  | Door DoorColor    -- ^ Door of a given color.
  | Button DoorColor  -- ^ A button that opens/toggles
                      -- all doors of the same color.
  | Exit              -- ^ An exit.
  -- extra tile types
  | Trap Direction    -- ^ A trap tile. Once character steps on this tile
                      -- a wall appears next to them.
  | Void              -- ^ A void tile, something to put outside level map.
  deriving (Eq)

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq)

-- | Available door and button colors.
data DoorColor
  -- standard colors
  = Red | Green | Blue
  -- extra colors
  | Pink | Purple | Yellow | Cyan | Orange
  | White | Gray | Black | Brown
  | DarkRed | DarkGreen
  | LightBlue | LightYellow
  deriving (Eq)

-- | Convert door color to CodeWorld's color.
doorColor :: DoorColor -> CodeWorld.Color
doorColor dc =
  case dc of
    Red         -> CodeWorld.red
    Blue        -> CodeWorld.blue
    Green       -> CodeWorld.green
    Purple      -> CodeWorld.purple
    Yellow      -> CodeWorld.brown
    Pink        -> CodeWorld.pink
    Brown       -> CodeWorld.brown
    Orange      -> CodeWorld.orange
    Cyan        -> CodeWorld.RGB 0.0 1.0 1.0
    White       -> CodeWorld.white
    Gray        -> CodeWorld.gray
    Black       -> CodeWorld.black
    DarkRed     -> CodeWorld.dark CodeWorld.red
    DarkGreen   -> CodeWorld.dark CodeWorld.green
    LightBlue   -> CodeWorld.light CodeWorld.blue
    LightYellow -> CodeWorld.light CodeWorld.yellow


-- | Coordinates on a level map.
data Coords = Coords Int Int

-- | Coordinates on a level map as a tuple.
type TupleCoords = (Int, Int)

toTupleCoords :: Coords -> TupleCoords
toTupleCoords (Coords i j) = (i, j)

-- | Level author name.
type Author = Text

-- | A level map with initial coordinates.
data Level = Level
  Author            -- Author of the level
  Coords            -- Start coordinates of the player.
  (Coords -> Tile)  -- Level map.
  [DoorColor]       -- Doors, opened on start.

firstLevel :: Level
firstLevel = level2


-- | A list of all level maps.
allLevels :: [Level]
allLevels =
  [ level4
  , level6
  -- , level7
  -- , level8
  -- , level9_1
  -- , level9_3
  -- , level17
  -- , level19
  ]

-- | Author: Viacheslav Goreev
level2 :: Level
level2 = Level "Viacheslav Goreev"
  (Coords (-8) (-2)) (hardLevel . toTupleCoords) [Blue, Red, Orange, Purple]
  where
    hardLevel :: TupleCoords -> Tile
    hardLevel (0, 0) = Exit
    --
    hardLevel (-6, -6) = (Button Blue)
    hardLevel (0, -8) = (Door Blue)
    hardLevel (3, 1) = (Door Blue)
    --
    hardLevel (8, -2) = (Button Red)
    hardLevel (-8, 0) = (Door Red)
    --
    hardLevel (-2, 8) = (Button Orange)
    hardLevel (0, 5) = (Door Orange)
    hardLevel (8, 0) = (Door Orange)
    --
    hardLevel (8, 2) = (Button Purple)
    hardLevel (0, 8) = (Door Purple)
    hardLevel (-4, 5) = (Door Purple)
    --
    hardLevel (x, y)
      | abs x > 10 || abs y > 10 = Wall
      | radius < 3 = Floor
      | x == 0 || y == 0 = Wall
      | (radius > 3 && radius < 4) || (radius > 6 && radius < 7) || radius >= 10 = Wall
      | radius < 10 = Floor
      | otherwise = Void
        where
          radius :: Double
          radius = sqrt((fromIntegral x)^2 + (fromIntegral y)^2)

-- | Author: Kamil Alimov
level4 :: Level
level4 = Level "Kamil Alimov"
  (Coords (-1) 7) newLevelMap [Blue, Red, Orange, Purple]
  where
    newLevelMap :: Coords -> Tile
    newLevelMap (Coords i j)
     | (i, j) == (-5, 2) = Door Red
     | (i, j) == (5, 2) = Door Purple
     | (i, j) == (-5, 3) = Button Blue
     | (i, j) == (5, 3) = Button Red
      | abs i > 9 || abs j > 9
        || abs i == 6 && j > 1 && j < 5
        || abs i == 4 && j > 1 && j < 5
        || abs i == 5 && j == 4
       -- || abs i < 6 &&  abs i > 2 && j > 1 && j < 5 &&
        ||  (i == -2 || i == 2) && j == -4
         ||  (i == -1 || i == 1) && j == -5
        || abs i < 5 && abs i > -1 && j < -1 && j > -4 = Wall
      | (i, j) == (0, -6) = Door Blue
      | (i, j) == (-4, 3) = Door Blue
      | (i, j) == (0, 1) = Button Purple
      | (i, j) == (0, -4) = Exit
      | otherwise = Floor

level6 :: Level
level6 = Level "Aliya Zagidullina"
  (Coords (-6) 9) level []
  where
    level :: Coords -> Tile
    level (Coords i j)
       | i==(-10) = Wall
       | i==10 = Wall
       | j==(-10) = Wall
       | j==10 = Wall
       | i == 0 && (j/=7 && j/=8 && j>0) = Wall
       | j == 0 &&  i>0 = Wall
       | j == 0 &&  i==0 = Door Red
       | i>(-9) && i<(-1) && (j==7 || j==8) = Wall
       | i>(-2) && i<1 && (j==7 || j==8) = Door Blue
       | i==(-1) && j==6 = Wall
       | (i==(-2) || i==(-3)) && (j==6 || j==5) = Door Red
       | i==(-1) && j==5 = Door Red
       | j<7 && j>3 && (i==(-8) || i==(-7) || i==(-6)) = Wall
       | i>(-9) && i<(-6) && (j>(-8) && j<2) = Wall
       | i==(-9) && (j>(-8) && j<2) = Door Green
       | i==(-6) && j==(-2) = Wall
       | i==(-6) && j==(-3) = Door Red
       | i==(-5) && j==(-3) = Wall
       | i==(-5) && j==(-4) =  Door Red
       | i==(-4) && j==(-4) = Wall
       | i==(-4) && j==(-5) = Door Red
       | i==(-3) && j==(-5) = Door Green
       | i==(-3) && j==(-6) = Door Red
       | i==(-2) && j==(-6) = Wall
       | i==(-2) && j==(-7) = Door Red
       | i==(-1) && j==(-7) = Wall
       | i==(-1) && j==(-8) = Door Red
       | i==(0) && j==(-8) = Wall
       | i==(0) && j==(-9) = Door Red
       | i==(0) && j==(-9) = Door Green
       | i==(-7) && j==(-8) = Button Red
       | i==(0) && j==(-1) = Button Green
       | i==1 && j==(-2) = Button Green
       | i==2 && j==(-3) = Button Green
       | i==3 && j==(-4) = Button Green
       | i==2 && j==(-5) = Button Green
       | i==1 && j==(-6) = Button Green
       | i==0 && j==(-7) = Button Green
       | i==4 && j<0 && j/=(-9) = Door Green
       | i==5 && j<0 && j/=(-9) = Wall
       | (i==4 || i==5) && j==(-9) = Door Red
       | i==9 && j==(-1) = Button Blue
       | i==9 && j==1 = Exit
       | otherwise = Floor

level7 :: Level
level7 = Level "Igor Vakhula"
  (Coords (-9) (-9)) (tileMap . toTupleCoords)  [Pink, Brown, Blue]
  where
    -- | tile map defining
    tileMap :: TupleCoords -> Tile
    tileMap (-10, _) = Wall
    tileMap (_, -10) = Wall
    tileMap (10, _)  = Wall
    tileMap (_, 10)  = Wall
    -- buttons
    tileMap (-5, 9)  = Button Pink
    tileMap (-1, 9)  = Button Brown
    tileMap (3, 9)   = Button Blue
    -- doors
    tileMap (-4, -9) = Door Pink
    tileMap (0, -9)  = Door Brown
    tileMap (4, -9)  = Door Blue
    -- room's wall restrictions XD
    tileMap (-4, _)  = Wall
    tileMap (0, _)   = Wall
    tileMap (4, _)   = Wall
    -- exit
    tileMap (9, 0)   = Exit
    --
    tileMap _        = Floor

level8 :: Level
level8 = Level "Pavel Vybornov"
  (Coords 1 1) (customMap . toTupleCoords) [Blue, Red, Orange, Purple]
  where
    -- | Used to set the tiles on the map
    customMap :: TupleCoords -> Tile
    customMap (i, j)
      | abs i == 10 || abs j == 10 || (abs i == 8 && abs j <= 8)
        || (abs i <= 8 && abs j == 8 && (not (i == 6 && j == 8)))
        || (abs i == 6 && abs j <= 6) || (abs i == 4 && abs j <= 4)
        || (abs i <= 6 && abs j == 6 && (not (i == 5 && j == 6)))
        || (abs i <= 4 && abs j == 4 && (not (i == 3 && j == 4)))
        || (abs i == 2 && abs j <= 2) || (abs i <= 2 && j == (-2))
        || (j == 9 && i == 9) || (j == 6 && i == 7)  || (j == 4 && i == 5)
        || (j == 2 && i == 3) || (j == 1 && i == 0) || (j == 0 && i == 0)
        || (j == 2 && i == 0) || (j == 2 && i == (-1)) = Wall
      | (i == (-1) && j == 1) = Button Red
      | (i == 3 && j == 1) = Button Green
      | (i == 5 && j == 3) = Button Blue
      | (i == 7 && j == 5) = Button Pink
      | (i == 9 && j == 8) = Button Purple
      | (i == 1 && j == 2) = Door Red
      | (i == 3 && j == 4) = Door Green
      | (i == 5 && j == 6) = Door Blue
      | (i == 6 && j == 8) = Door Pink
      | (i == 7 && j == 9) = Door Purple
      | j == 9 && i == 8 = Exit
      | otherwise = Floor

level9_1 :: Level
level9_1 = Level "Niyaz Fahretdinov"
  (Coords 0 0) (customMap . toTupleCoords) []
  where
    Map _ customMap = map2
    --
    -- | Second level of the game
    map2 :: Map
    map2 = Map ((-2, -2), (10, 4)) level
                    <-- boxOf (-1, -1) (9, 3) Wall
                    <-- boxOf (-2, -2) (10, 4) Floor
      where
        level (8, _) = Exit
        --
        level (5, _) = Button Green
        level (7, _) = Door Green
        --
        level (1, 0) = Wall
        level (1, 1) = Wall
        --
        level (3, 2) = Wall
        level (3, 1) = Wall
        --
        level _      = Floor

level9_3 :: Level
level9_3 = Level "Niyaz Fahretdinov"
  (Coords 0 0) (customMap . toTupleCoords) [Red]
  where
    Map _ customMap = map4
    -- | 4th level of the game
    map4 :: Map
    map4 = moveOrigin (0, -4) $ Map ((-10, -10), (10, 10)) level
                <-- boxOf (-10, -10) (10, 10)  Exit
                <-- boxOf (-9, -9) (9, 9)      (Door Yellow)
                <-- boxOf (-8, -8) (8, 8)      (Door Green)
                <-- boxOf (-7, -7) (7, 7)      (Door Blue)
                <-- boxOf (-6, -6) (6, 6)      (Door Red)
                --
                <-- boxOf (-5, -5) (5, 5)      Wall
                <-- boxOf (-4, -4) (4, 4)      Wall
                <-- boxOf (-3, -3) (3, 3)      Wall
                --
                <-- xLineOf 0 (3, 5)           Floor
                <-- xLineOf 0 (-3, -5)         Floor
                --
                <-- yLineOf 0 (3, 5)           Floor
                <-- yLineOf 0 (-3, -5)         Floor
      where
        level (x, y)
            | abs x == 2 && abs y == 2 =  Button Green
            | abs x + abs y < 2        =  Button Yellow
            | (x + y) `mod` 2 == 0     =  Button Red
            | otherwise                =  Button Blue

level12 :: Level
level12 = Level "Arina Fedorovskaya"
  (Coords 8 (-9)) level []
  where
    level :: Coords -> Tile
    -- | Frame for map
    level (Coords (-10) _) = Wall
    level (Coords 10 _) = Wall
    level (Coords _ (-10)) = Wall
    level (Coords _ 10) = Wall
    -- | Walls
    level (Coords x y)
     | (y == (-5) && x >= 7) || y == -4 && x == 1
     || (y < (-6) && y > (-8)) && x == 1
     || ((x > 6 || x < 6) && (y == -8))
     || ((y == -3) && (x < -8 || x > -5))
     || ((x == 7) &&  (y == (-6)))
     || (x == -3) && (y < -3 && y > -7)
     || (y == 0) && x < 7
     || (y == -1 && x == 6)
     || (x == 5 || x == 4) && (y < 10 && y > 6)
     || (x < 10 && x > 7) && y == 5
     || (x < 7 && x > 3) && y == 5
     || (y < 9 && y > 1) && (x == -7 || x == -1)
     || (y < 8 && y > 0) && (x == -4 || x == 2) = Wall
    -- | Buttons
    level (Coords (-8) (-9)) = Button Red
    level (Coords 9 (-6)) = Button Red
    level (Coords x y)
      | ((x == 3 || x == 4 || x == 5) && (y == -4)) = Button Blue
    level (Coords x y)
      | (x == -9) && (y < -3 && y > -8) = Button Pink
    level (Coords x y)
      | (x == -9) && (y < 0 && y > -3) = Button Orange
    level (Coords (-9) 9) = Button Green
    -- | Doors
    level (Coords x y)
      | ((x == 1) && (y == -5 || y == -6)) = Door Red
    level (Coords x y)
      | (x > -9 && x < -4) && (y == -3) = Door Blue
    level (Coords x y)
      | (x < 10 && x > 6) && (y == 0) = Door Orange
    level (Coords 5 6) = Door Pink
    level (Coords 4 6) = Door Green
    level (Coords x y)
      | (x > -8 && x < 6) && (y == 9) = Door Green
    -- | Traps
    level (Coords 6 (-7)) = Trap DirDown
    level (Coords 8 (-7)) = Trap DirLeft
    level (Coords (-4) (-7)) = Trap DirRight
    level (Coords 7 (-2)) = Trap DirLeft
    level (Coords 7 4) = Trap DirUp
    level (Coords 1 8) = Trap DirRight
    level (Coords (-5) 8) = Trap DirRight
    level (Coords (-2) 1) = Trap DirRight
    level (Coords (-8) 1) = Trap DirRight
    -- | Exit
    level (Coords x y)
     | (x == 8 || x == 7) && (y == 8 || y == 7) = Exit
    -- | Floor
    level _ = Floor


level17 :: Level
level17 = Level "Rim Rakhimov"
  (Coords 0 0) tileAt2 []
  where
    -- | Tile at the given coordinate (defines the map for the task 2)
    tileAt2 :: Coords -> Tile
    -- exits
    tileAt2 (Coords 9 (-1)) = Exit
    tileAt2 (Coords 8 (-1)) = Exit
    -- doors
    tileAt2 (Coords 6 (-1)) = Door Red
    tileAt2 (Coords 6 (-8)) = Door Blue
    tileAt2 (Coords 5 3) = Door Green
    tileAt2 (Coords (-5) (-5)) = Door Gray
    tileAt2 (Coords (-4) (-6)) = Door Gray
    tileAt2 (Coords (-3) (-7)) = Door Gray
    tileAt2 (Coords (-2) (-8)) = Door Gray
    tileAt2 (Coords (-1) (-9)) = Door Gray
    tileAt2 (Coords 0 (-6)) = Door Orange
    -- buttons
    tileAt2 (Coords 5 (-8)) = Button Red
    tileAt2 (Coords 7 3) = Button Blue
    tileAt2 (Coords (-5) (-8)) = Button Blue
    tileAt2 (Coords (-4) 0) = Button Green
    tileAt2 (Coords (-8) 7) = Button Gray
    tileAt2 (Coords (-4) (-7)) = Button Gray
    tileAt2 (Coords 5 2) = Button Orange
    -- floors
    tileAt2 (Coords i j)
      | j == -9 && (i >= -8 && i <= -3 || i >= 1 && i <= 4 || i >= 6 && i <= 9) = Floor
      | j == -8 && (i == -9 || i >= -7 && i <= -1 || i >= 1 && i <= 3 || i >= 5 && i <= 9) = Floor
      | j == -7 && (i >= -9 && i <= -8 || i >= -6 && i <= 0 || i >= 2 && i <= 4 || i >= 7 && i <= 9) = Floor
      | j == -6 && (i >= -9 && i <= -7 || i >= -5 && i <= 9) = Floor
      | j == -5 && (i >= -9 && i <= -2 || i >= 0 && i <= 1 || i >= 3 && i <= 9) = Floor
      | j == -4 && (i == -9 || i == -7 || i == 1 || i == 3 || i >= 5 && i <= 9) = Floor
      | j == -3 && (i == -9 || i >= -7 && i <= -1 || i == 1 || i == 3 || i >= 5 && i <= 9) = Floor
      | j == -2 && (i == -9 || i == -1 || i == 1 || i == 3) = Floor
      | j == -1 && (i == -9 || i >= -7 && i <= -1 || i == 1 || i >= 3 && i <= 9) = Floor
      | j == 0 && (i == -9 || i >= -7 && i <= -2 || i >= 0 && i <= 1) = Floor
      | j == 1 && (i == -9 || i >= -7 && i <= -1 || i >= 1 && i <= 3 || i >= 5 && i <= 9) = Floor
      | j == 2 && (i >= -3 && i <= -1 || i == 3 || i == 9) = Floor
      | j == 3 && (i >= -9 && i <= -5 || i >= -3 && i <= 1 || i >= 3 && i <= 7 || i == 9) = Floor
      | j == 4 && (i == -7 || i == -5 || i == -3 || i == 3 || i == 7 || i == 9) = Floor
      | j == 5 && (i >= -9 && i <= -7 || i == -5 || i >= -3 && i <= 1 || i >= 3 && i <= 5 || i == 7 || i == 9) = Floor
      | j == 6 && (i >= -9 && i <= -4 || i == 3 || i == 5 || i == 7 || i == 9) = Floor
      | j == 7 && (i >= -9 && i <= -7 || i == -4 || i >= -2 && i <= 3 || i == 5 || i == 7 || i == 9) = Floor
      | j == 8 && (i >= -9 && i <= -7 || i == -4 || i == 3 || i == 5 || i == 9) = Floor
      | j == 9 && (i >= -9 && i <= -7 || i >= -4 && i <= 3 || i >= 5 && i <= 9) = Floor
    -- walls
    tileAt2 _ = Wall

level19 :: Level
level19 = Level "Bykov Artemii"
  (Coords (-worldSize + 1) (-worldSize + 1)) (complexMap . toTupleCoords) []
  where
    -- | Global world size. For worldSize=8 coords should be between -8 and 8.
    worldSize :: Int
    worldSize = 8

    -- | Complex map for further solutions.
    complexMap :: TupleCoords -> Tile
    complexMap (x, y)
    -- world-bounding tiles
      | x == worldSize + 1 || x == -worldSize || y == worldSize + 1 || y == -worldSize = Wall
    -- actual world tiles
      | (x, y) == (3, 3) = Exit
      | x == 2 && x > -worldSize && x < worldSize || y == 2 = Door Green
      | (x, y) == (4, 4) = Button Green
      | (x, y) == (1, 1) = Button Green
      | (x, y) == (-6, -6) = Button Blue
      | (x, y) == (-7, -6) || (x, y) == (-7, -5) || (x, y) == (-6, -5) ||
        (x, y) == (-5, -5) || (x, y) == (-5, -6) || (x, y) == (-5, -7)
        = Door Blue
    -- catch-all for floor tiles
      | x > -worldSize && x < worldSize + 1 && y > -worldSize && y < worldSize + 1 = Floor
    -- catch-all for out-of-world tiles
      | otherwise        = Void

--

-- | Format for the game maps.
-- Bounds for the level
-- and the functions that gives tile for every coordinate
data Map = Map Bounds (TupleCoords -> Tile)

type Bounds = ((Int, Int), (Int, Int))

-- | Creates a bounded by the coordinates map made of only Floor
emptyMap :: TupleCoords -> TupleCoords -> Map
emptyMap start end = Map (start, end) (const Floor)

-- | An map operation that helps mutate maps
-- it's a function from TupleCoords to Maube Tile
-- when it is Nothing, the level is not changed at this coords
-- when it is Just someTile, the level tile changes to someTile at the coords
data MapOperation = Tiling (TupleCoords -> Maybe Tile)

-- | An operation for applying the peration to a map
(<--) :: Map -> MapOperation -> Map
(Map bounds level) <-- (Tiling f) = Map bounds newLevel
  where
    newLevel coords =
      case f coords of Nothing   -> level coords
                       Just tile -> tile

-- | An operation that draws a (x = a) line bounded at [sy; ey]
xLineOf :: Int -> TupleCoords -> Tile -> MapOperation
xLineOf a (sy, ey) tile = Tiling lined
  where
    bot = min sy ey
    top = max sy ey

    lined (i, j)
      | i == a && j >= bot && j <= top = Just tile
      | otherwise                    = Nothing

-- | An operation that draws a (y = a) line bounded at [sx; ex]
yLineOf :: Int -> TupleCoords -> Tile -> MapOperation
yLineOf y xBounds tile = Tiling (\(i, j) -> xOp (j, i))
  where
    Tiling xOp = xLineOf y xBounds tile

-- | An operation that changes tile at (x, y) to specified one
tileAt :: TupleCoords -> Tile -> MapOperation
tileAt (x, y) tile = Tiling point
  where
    point (i, j)
      | i == x && j == y = Just tile
      | otherwise        = Nothing

-- | An operation thet draw an empty suare
boxOf :: TupleCoords -> TupleCoords -> Tile -> MapOperation
boxOf (sx, sy) (ex, ey) tile = Tiling box
  where
    l = min sx ex
    r = max sx ex
    b = min sy ey
    t = max sy ey

    box (i, j)
      | (i == l || i == r) && j >= b && j <= t = Just tile
      | (j == b || j == t) && i >= l && i <= r = Just tile
      | otherwise                            = Nothing


-- | An operation that keeps the level and the bounds the same
-- But moved by (x, y)
moveOrigin :: TupleCoords -> Map -> Map
moveOrigin (x, y) (Map (oldStart, oldEnd) level) = Map newBounds newLevel
  where
    newBounds = ( (fst oldStart + x, snd oldStart + y)
                , (fst oldEnd + x, snd oldEnd + y)
                )
    newLevel (i, j) = level (i - x, j - y)
