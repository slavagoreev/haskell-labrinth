{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wname-shadowing #-}
module EscapeTheRoom where

import           CodeWorld
import qualified Data.Text as T
import           EscapeTheRoom.Levels


mapLength :: Int
mapLength = 20

-- | Game representation structure
data Game = Game Coords Level

-- | Type of the `activityOf` function.
type ActivityOf world
  = world
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()

-- | Add a "reset" on Esc button press to an existing activityOf.
withReset :: ActivityOf world -> ActivityOf world
withReset
  activityOf
  initialState
  handleEvent
    = activityOf initialState handleEvent'
      where
        handleEvent' (KeyPress "Esc") _ = initialState
        handleEvent' event world = handleEvent event world


-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = StartScreen  -- ˆ Start screen.
  | GameOn world -- ˆ Game is on with 'world' state.

-- | Add start screen to 'activityOf'.
withStartScreen :: ActivityOf (WithStartScreen world) -> ActivityOf world
withStartScreen activityOf initialState handleEvent drawGame
  = activityOf StartScreen handleEvent' drawGame'
    where
      handleEvent' (KeyPress " ") StartScreen = GameOn initialState
      handleEvent' _ StartScreen = StartScreen
      handleEvent' event (GameOn world) = GameOn (handleEvent event world)

      drawGame' StartScreen
        = scaled 2 2 (lettering "Escape the Room")
        <> translated 0 (-3) (lettering "[Press Space to start]")
        <> colored (lighter 0.5 brown) (solidRectangle 100 100)
      drawGame' (GameOn world) = drawGame world


preopenDoors :: Level -> (Coords -> Tile)
preopenDoors (Level _ _ levelMap []) = levelMap
preopenDoors (Level a b levelMap (currentDoorColor:rest)) 
  = preopenDoors (Level a b (updateDoorState (doorColor currentDoorColor) . levelMap) rest)

-- | Initialise game 'State' for a given 'LevelMap'.
initLevelMap :: Level -> Game
initLevelMap level@(Level a startPoint _ doorsOpened) 
  = Game startPoint (Level a startPoint (preopenDoors level) doorsOpened)

-- | Is current level complete given some game 'State'?
isLevelComplete :: Game -> Bool
isLevelComplete (Game playerCoords (Level _ _ levelMap _))
  | isExit currentTile    = True
  | otherwise             = False
  where
    currentTile = levelMap playerCoords


data WithLevel world = WithLevel world Int

withManyLevels 
  :: [level] -- ˆ A list of levels.
  -> (level -> world) -- ˆ Initialise world for level.
  -> (world -> Bool) -- ˆ Is this level complete?
  -> ActivityOf (WithLevel world) -- ˆ 'ActivityOf'.
  -> ActivityOf world
withManyLevels levels initLevelMap isLevelComplete activityOf initialState handleEvent drawGame 
  = activityOf initialState' handleEvent' drawGame'
    where
      initialState' = WithLevel initialState 0
      handleEvent' event (WithLevel game currentLevelIdx)
        | isLevelComplete game
-- withManyLevels [level] initLevelMap isLevelComplete activityOf initialState handleEvent drawGame 
--   = withNextLevel level initLevelMap isLevelComplete activityOf initialState handleEvent drawGame
-- withManyLevels (nextLevel:rest) initLevelMap isLevelComplete activityOf initialState handleEvent drawGame 
--   -- = withNextLevel nextLevel initLevelMap isLevelComplete (withManyLevels rest initLevelMap isLevelComplete) initialState handleEvent drawGame 
--   = withNextLevel nextLevel initLevelMap isLevelComplete (withManyLevels rest initLevelMap isLevelComplete) initialState handleEvent drawGame 
--   -- = withManyLevels rest initLevelMap isLevelComplete (withNextLevel nextLevel initLevelMap isLevelComplete activityOf initialState handleEvent drawGame) -- initialState handleEvent drawGame 

run :: IO ()
run = withManyLevels allLevels initLevelMap isLevelComplete (withStartScreen (withReset activityOf)) initialState handleEvent drawGame
-- run = withNextLevel level6 initLevelMap isLevelComplete (withNextLevel level4 initLevelMap isLevelComplete (withStartScreen (withReset activityOf))) initialState handleEvent drawGame
  where
    initialState = initLevelMap firstLevel


----------------------
-- Helper functions --
----------------------


isButton :: Tile -> Bool
isButton (Button _) = True
isButton _ = False

isExit :: Tile -> Bool
isExit Exit = True
isExit _ = False

isDoor :: Tile -> Bool
isDoor (Door _) = True
isDoor _ = False

    
canTileBeVisited :: Tile -> Bool
canTileBeVisited Wall = False
canTileBeVisited (Door _) = False
canTileBeVisited Void = False
canTileBeVisited _ = True

------------------
-- Render unils --
------------------

drawText :: Char -> Picture
drawText char = lettering (T.singleton char)


-- | Draw the player at the specified Coords
drawPlayer :: Coords -> Picture
drawPlayer (Coords x y) = translated (fromIntegral x) (fromIntegral y) (drawText '\xC6C3')

-- | Render a single tile given its type.
drawTile :: Tile -> Picture
drawTile Floor = colored (lighter 0.4 gray) (solidRectangle 0.95 0.95)
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Exit 
  = colored black (solidRectangle 0.25 0.25) 
  <> colored red (solidRectangle 0.80 0.80) 
  <> drawTile Wall
drawTile (Door color) 
  = colored (doorColor color) (solidCircle 0.15) 
  <> translated (-0.15) (-0.05) (scaled 1.25 0.75 $ colored white $ drawText '\x25AF')
  <> drawTile Wall
drawTile (Button color) 
  = colored (doorColor color) (solidCircle 0.3) 
  <> drawTile Floor
drawTile Void = blank


-------------------
-- Map rendering --
-------------------


-- | Recursively pasess from, to coordinates in drawFn
drawFromTo 
  :: (Int, Int)   -- Range in which we need to apply our DrawFn
  -> (Int -> Picture) -- Draw function that accepts j coordinate
  -> Picture
drawFromTo (from, to) drawFn
  | from > to = blank
  | otherwise = drawFn from <> drawFromTo (from + 1, to) drawFn
    
drawRow
  :: (Coords -> Tile)   -- level map
  -> (Int, Int) -- length of the row
  -> Int            -- current row
  -> Picture
drawRow levelFn (from, to) i = drawFromTo (from, to) drawTileAt
  where
    drawTileAt j = translated x y (drawTile (levelFn (Coords i j)))
      where
        x = fromIntegral i
        y = fromIntegral j

-- | Render the level with player
drawGame :: Game -> Picture
drawGame (Game playerCoords (Level _ _ levelFn _)) 
  = drawPlayer playerCoords 
  <> drawFromTo (-halfLength, halfLength + 1) (drawRow levelFn (-halfLength, halfLength + 1))
  where 
    halfLength = mapLength `div` 2




-------------------------
-- Game event handling --
-------------------------

-- For each point in range of level open the doors of activated color
updateDoorState :: Color -> Tile -> Tile
updateDoorState color (Door dc)
  | color == doorColor dc = Floor
  | otherwise = Door dc
updateDoorState _ tile = tile


-- | Updates the game state with open doors
toggleDoors :: Color -> Game -> Game
toggleDoors color (Game playerCoords (Level author startCoords levelFn doorsOpened)) =
  Game playerCoords (Level author startCoords (updateDoorState color . levelFn) doorsOpened)
  

activateExit :: Tile -> Tile
activateExit Exit = Exit
activateExit tile = tile

endGame :: Game -> Game
endGame (Game playerCoords (Level author startCoords levelFn doorsOpened)) =
  Game playerCoords (Level author startCoords (activateExit . levelFn) doorsOpened)


pressTheButton :: Tile -> Game -> Game
pressTheButton (Button c) = toggleDoors (doorColor c)
pressTheButton _ = id


updateWorld :: Game -> Game
updateWorld game@(Game playerCoords (Level _ _ levelMap _))
  | isExit currentTile    = endGame game
  | isButton currentTile  = pressTheButton currentTile game
  | otherwise             = game
  where
    currentTile = levelMap playerCoords


tryMoveHelper :: Coords -> Game -> Game
tryMoveHelper nextPlayerCoords (Game playerCoords level@(Level _ _ levelMap _))
  | isExit currentTile        = Game playerCoords level
  | canTileBeVisited nextTile = updateWorld (Game nextPlayerCoords level)
  | otherwise                 = Game playerCoords level
  where
    currentTile = levelMap playerCoords
    nextTile = levelMap nextPlayerCoords


getNextPlayerCoords :: Coords -> Direction -> Coords
getNextPlayerCoords (Coords x y) DirUp = Coords x (y + 1)
getNextPlayerCoords (Coords x y) DirLeft = Coords (x - 1) y
getNextPlayerCoords (Coords x y) DirDown = Coords x (y - 1)
getNextPlayerCoords (Coords x y) DirRight = Coords (x + 1) y
    
-- | Try to move the player in the specified direction.
tryMove :: Direction -> Game -> Game
tryMove direction game@(Game playerCoords _) = tryMoveHelper nextPlayerCoords game
  where
    nextPlayerCoords = getNextPlayerCoords playerCoords direction


    
-- | Handle an input event.
handleEvent :: Event -> Game -> Game
handleEvent (KeyPress "Up") = tryMove DirUp
handleEvent (KeyPress "Down") = tryMove DirDown
handleEvent (KeyPress "Left") = tryMove DirLeft
handleEvent (KeyPress "Right") = tryMove DirRight
handleEvent _ = id

