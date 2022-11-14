module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

main :: IO ()
main = do 
    images <- loadImages
    playIO (InWindow "Asteroids" (800, 800) (0, 0)) -- Or FullScreen
      black                  -- Background color
      25                     -- Frames per second
      initialState           -- Initial state
      (view images)          -- View function
      input                  -- Event function
      step                   -- Step function

loadImages :: IO([Picture])
loadImages = do
    background <- loadBMP "app/background.bmp"
    asteroid   <- loadBMP "app/asteroid.bmp"
    spaceship  <- loadBMP "app/spaceship.bmp"
    let images = background : asteroid : spaceship : []
    return $ images