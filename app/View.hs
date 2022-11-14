-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller

-- view function 
view :: [Picture] -> GameState -> IO Picture
view bmps gs = do
  ts <- topScores
  let topscore = ts
  return (viewPure bmps topscore gs)

viewPure :: [Picture] -> [Int] -> GameState -> Picture
viewPure picture topscore gs = pictures [background, bullet, player, asteroid, explosions, msg, score]
 where
  background = scale 2 2 (picture!!0)
  asteroid   = viewAsteroids (picture!!1) gs
  bullet     = viewBullets gs
  player     = viewPlayer (picture!!2) gs
  explosions = viewExplosions gs
  score      = viewScore gs
  msg        = viewPause topscore gs
       
-- spaceship picture
spaceship :: Picture
spaceship = color white (polygon [(0,0),(-40,40),(-40,40),(0,0)])

-- bullet picture
bullet :: Picture
bullet = color white (Circle(6)) 

-- the position of the ship which the user controls.
position_sps :: Ship -> Picture
position_sps (Ship (Collide (x,y) z) v) = translate x y spaceship

-- viewing the player.
viewPlayer :: Picture -> GameState -> Picture
viewPlayer image gs
 | ship gs == NoShip = Blank 
 | otherwise             = translate x y (rotate (x+y) (scale 0.4 0.4 image))
    where (x,y) = position (collide (ship gs))

viewPause :: [Int] -> GameState -> Picture
viewPause topscores gs = viewPause' topscores gs (phase gs)

viewPause' :: [Int] -> GameState -> Phase -> Picture
viewPause' ts gs Start     = 
  pictures [scale 0.2 0.2 (translate (-1700) 1500 (color white (text "press space to start the game"))), 
            scale 0.2 0.2 (translate (-1700) 1300 (color white (text "use w a s d to move"))), 
            scale 0.2 0.2 (translate (-1700) 1100 (color white (text "arrow keys to shoot")))]
-- helper function
viewPause' ts gs GameOver  = 
  pictures [scale 0.2 0.2 (translate (-1700) 1500 (color white (text "press r to retry"))), 
            scale 0.2 0.2 (translate (-1700) 1100 (color white (text ((map show ts)!!0))))]
viewPause' ts gs Pause     = 
  scale 0.2 0.2 (translate (-1700) 1500 (color white (text "press p to unpause the game")))
viewPause' ts gs Playing   =
  scale 0.2 0.2 (translate (-1700) 1500 (color white (text "press p to pause the game and r to restart")))

-- view Asteroids
viewAsteroids :: Picture -> GameState -> Picture
viewAsteroids image gs = 
  pictures [translate a b (rotate (a/2+b/2) (scale 0.15 0.15 image)) | (a,b) <- x]
    where x = map (position.collideAsteroid) (asteroids gs)

-- view Explosions
viewExplosions :: GameState -> Picture
viewExplosions gs = 
  pictures (map viewParticles (explosions gs))

-- view each particle of an explosion
viewParticles :: [Particle] -> Picture
viewParticles explosion = 
  pictures [translate x y (color (makeColor 1 1 0 (1-(age/2))) (circleSolid(z))) | 
            Particle (Collide(x,y) z) _ age _ <- explosion]

viewBullets :: GameState -> Picture
viewBullets gs = 
  pictures [translate x y (rotate (rotation v) (scale 40 10(color cyan (line [(0,-1),(0,1)] )))) | 
            (Bullet (Collide (x,y) z) v)<- bullets gs]

viewScore :: GameState -> Picture
viewScore gs = 
  pictures [scale 0.2 0.2 (translate (-1500) 1600 (color white (text "Score"))), 
            scale 0.2 0.2 (translate (-1500) 1600 (color white (text (show (score gs)))))]

rotation :: Velocity -> Float
rotation (vx,vy) = (-1)*(atan(vy/vx))/(pi)*180+90