-- | This module contains the data types
--   which represent the state of the game
module Model where

 -- GameState data type 
data GameState = GameState {
                  phase        :: Phase
                , ship         :: Ship
                , asteroids    :: [Asteroid]
                , bullets      :: [Bullet]
                , timeInterval :: Float
                , score        :: Int
                , explosions   :: [Explosion]
                 }

 -- Phase data type
data Phase = Playing
           | GameOver
           | Start
           | Pause
           deriving (Eq,Show)

 -- Ship data type
data Ship = Ship {collide :: Collide, velocity :: Velocity} 
          | NoShip
           deriving (Eq,Show)

-- bullet data type
data Bullet = Bullet {collideBullet :: Collide, velocityBullet :: Velocity} 
            deriving (Eq,Show)

 -- Collide data type
data Collide = Collide {position :: Location, radius :: Float} 
  deriving (Eq,Show)

type Location = (Float, Float)
type Velocity = (Float, Float)

data Asteroid = Asteroid {collideAsteroid:: Collide, velocityAsteroid :: Velocity}  
              deriving (Eq,Show)

data Particle = Particle Collide Velocity Float Float
              deriving (Eq,Show)

type Explosion = [Particle]

 -- initial game state (before the user starts the game)
initialState :: GameState
initialState = GameState Start NoShip [] [] 0 0 []

-- when the user starts the game.
startPlayingState :: GameState
startPlayingState = GameState Playing (Ship(Collide(0,0) 20) (0,0)) produceAsteroids [] 0 0 []
  where produceAsteroids = [Asteroid (Collide (-420,-420) 25) (5,7), 
                            Asteroid (Collide (420,-420) 25) (-5,5), 
                            Asteroid (Collide (-420,420) 25) (8,-4), 
                            Asteroid (Collide (420,420) 25) (-7,-6)]