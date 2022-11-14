-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import System.IO

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gamestate
 | phase gamestate == Start     = 
    return $ initialState
 | phase gamestate == Pause     = 
    return $ gamestate
 | phase gamestate == GameOver  = 
    return $ gamestate {ship = NoShip, timeInterval = 0, explosions = updateParticles seconds gamestate, asteroids = [], bullets = []}
 | phase gamestate == Playing && shipAllAsteroid gamestate = do
    saveScore gamestate
    return $ gamestate {phase = GameOver, ship = NoShip, timeInterval = 0}
 | phase gamestate == Playing && not (shipAllAsteroid gamestate) && timeInterval gamestate > 3 = produceAsteroids gamestate
 | otherwise = 
    return $ gamestate {ship = moveShip (ship gamestate),
                        score = (score gamestate) + checkAsteroids  (bullets gamestate) (asteroids gamestate),
                        asteroids = moveAsteroids (asteroids gamestate) (score gamestate) (ship gamestate) (bullets gamestate),
                        bullets = moveBullets (bullets gamestate),
                        timeInterval = timeInterval gamestate + seconds,
                        explosions = updateParticles seconds gamestate}

-- collision handling
-- we have two kinds of collision: 1) asteroid and ship 2) asteroid and bullets
shipAsteroid :: Ship -> Asteroid -> Bool
shipAsteroid NoShip _ = False
shipAsteroid ship asteroid = 
  distance (position (collide ship)) (position (collideAsteroid asteroid)) <= radius (collide ship) + radius (collideAsteroid asteroid)

shipAllAsteroid :: GameState -> Bool
shipAllAsteroid gamestate = any (\x -> shipAsteroid (ship gamestate) x) (asteroids gamestate)


asteroidAllBullet :: Asteroid -> [Bullet] -> (Bool, Location)
asteroidAllBullet _ [] = (False,(0,0))
asteroidAllBullet y (x:xs)
  | fst (asteroidBullet y x) = (True, position (collideAsteroid y))
  | otherwise          =  asteroidAllBullet y xs
  
asteroidBullet ::  Asteroid -> Bullet -> (Bool, Location)
asteroidBullet y x
  | distance (position (collideBullet x)) (position (collideAsteroid y)) <= radius (collideBullet x) + radius (collideAsteroid y) =
      (True, position (collideAsteroid y))
  | otherwise = (False, (0,0))
  
-- generate a random tuple with the two given numbers.
-- this function is used in the next function where we generate random asteroids.
randomTuple :: Int -> Float -> Float -> IO (Float, Float)
randomTuple 1 x y = do return (y,x)
randomTuple 0 x y = do return (x,y)

-- given a gamestate returns a gamestate with a new asteroid randomly placed.
produceAsteroids :: GameState -> IO GameState 
produceAsteroids gamestate = do 
  randomSpeed <- randomRIO (2, 5) :: IO Float
  let cornerPoints = [-400,400]
  random1 <- randomRIO (0,1) :: IO Int
  let randomAxis1 = cornerPoints !! random1
  randomAxis2 <- randomRIO (-350,350)
  random3 <- randomRIO (0,1) :: IO Int
  randomPlace <- randomTuple random3 randomAxis1 randomAxis2
  destinationX <- randomRIO (-200,200)
  destinationY <- randomRIO (-200,200)
  return $ gamestate {ship = moveShip (ship gamestate),
                      asteroids = Asteroid (Collide randomPlace 5) (unitV ((fst randomPlace - destinationX),(snd randomPlace - destinationY)))
                      :(moveAsteroids (asteroids gamestate) (score gamestate) (ship gamestate) (bullets gamestate)),
                      bullets = moveBullets (bullets gamestate),
                      score = (score gamestate) + checkAsteroids (bullets gamestate) (asteroids gamestate),
                      timeInterval = 0}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

inputKey :: Event -> GameState -> GameState
inputKey input@(EventKey c d e f) gs
  | c == SpecialKey KeySpace && d == Down && phase gs == Start = startPlayingState
  | elem c gameControlKeys                                     = updateGamestate input gs
  | elem c shootKeys && d == Down                              = gs {bullets = createBullet input gs : bullets gs}
  | elem c movementKeys                                        = gs {ship  = updateShipVelocity (ship gs) input gs}
  where
    gameControlKeys = [Char 'p', Char 'r']
    movementKeys = [Char 'w', Char 'a', Char 's', Char 'd']
    shootKeys = [SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyRight, SpecialKey KeyLeft]
inputKey _ gs = gs

-- gamestate control functions
updateGamestate :: Event -> GameState -> GameState
updateGamestate (EventKey (Char 'p') Down _ _) gs            
  | phase gs == Playing = gs {phase = Pause}
  | phase gs == Pause   = gs {phase = Playing}
  | otherwise                   = gs
updateGamestate (EventKey (Char 'r') Down _ _) gs            
 | phase gs == Pause       = gs
 | phase gs == GameOver    = startPlayingState 
 | otherwise               = initialState
updateGamestate _ gs = gs

-- | asteroid functions
moveAsteroids :: [Asteroid] -> Int -> Ship -> [Bullet] -> [Asteroid]
moveAsteroids [] _ _ _ = []
moveAsteroids (x@(Asteroid(Collide(xA,yA) rA) (vXA,vYA)):xs) score ship@((Ship(Collide(xP,yP) rP) (vXP, vYP))) bullets
  | fst((asteroidAllBullet x bullets)) || inGameWindow (xA,yA) (-500,500) == False = moveAsteroids xs score ship bullets 
  | fst(asteroidAllBullet x bullets) == False && score < 1000 = ((Asteroid (Collide (xA+vXA, yA+vYA) rA) (vXA,vYA)):(moveAsteroids xs score ship bullets)) 
  | otherwise                                                 = ((Asteroid (Collide (xA+vXA, yA+vYA) rA) newAstVelocity):(moveAsteroids xs score ship bullets)) 
  where
    speed = vectorL (vXA,vYA)
    directionAP = (xP-xA, yP-yA) :: Velocity 
    newAstVelocity = (fst (unitV directionAP) * speed, snd (unitV directionAP) * speed)

checkAsteroids :: [Bullet] -> [Asteroid] -> Int
checkAsteroids xs ys = length (filter fst (map (\y -> asteroidAllBullet y xs) ys)) * 100

-- ship control functions
updateShipVelocity :: Ship -> Event -> GameState -> Ship 
updateShipVelocity (Ship c (a,b)) (EventKey key Down _ _) gs 
 | key == (Char 'w') && phase gs == Playing = Ship c (a,b+7)
 | key == (Char 'a') && phase gs == Playing = Ship c (a-7,b)
 | key == (Char 's') && phase gs == Playing = Ship c (a,b-7)
 | key == (Char 'd') && phase gs == Playing = Ship c (a+7,b)
 | otherwise                                = Ship c (0,0)
updateShipVelocity (Ship c (a,b)) (EventKey key Up _ _) gs   
 | key == (Char 'w') && phase gs == Playing = Ship c (a,b-7)
 | key == (Char 'a') && phase gs == Playing = Ship c (a+7,b)
 | key == (Char 's') && phase gs == Playing = Ship c (a,b+7)
 | key == (Char 'd') && phase gs == Playing = Ship c (a-7,b)
 | otherwise                                = Ship c (0,0)
updateShipVelocity (Ship c v) _ _ = Ship c v
updateShipVelocity NoShip _ _     = NoShip

moveShip :: Ship -> Ship
moveShip NoShip = NoShip
moveShip (Ship(Collide (x,y) z) (a,b))
  | (inGameWindow (x,y) (-420,420)) = Ship(Collide (x+a,y+b) z) (a,b)
  | otherwise                       = Ship(Collide (0,0) z) (0,0)

-- bullet control functions
createBullet :: Event -> GameState -> Bullet
createBullet (EventKey key Down _ _) gs = Bullet (Collide playerP 10) (createBulletVelocity playerV key)
  where
    playerV = velocity (ship gs)
    playerP = position (collide (ship gs))

createBulletVelocity :: Velocity -> Key -> Velocity
createBulletVelocity (vX,vY) key 
 | key == (SpecialKey KeyUp)    = (vX*0.25, 10)
 | key == (SpecialKey KeyLeft)  = (-10, vY*0.25)
 | key == (SpecialKey KeyDown)  = (vX*0.25, -10)
 | key == (SpecialKey KeyRight) = (10, vY*0.25)

moveBullets :: [Bullet] -> [Bullet]
moveBullets [] = []
moveBullets ((Bullet(Collide(x,y) r) (vX, vY)):bs) 
  | inGameWindow(x,y) (-600, 600) = ((Bullet(Collide(x+vX, y+vY) r) (vX*1.05, vY*1.05)) : moveBullets bs)
  | otherwise                     = moveBullets bs

-- | particle functions
makeParticle :: Location -> Velocity -> Float -> Float -> Particle
makeParticle pos vel age exp = Particle (Collide pos 5) vel age exp

createExplosion :: Location -> Explosion
createExplosion (x,y) = makeParticle (x,y) (0, -14) 1 4 
                      : makeParticle (x,y) (0, 14) 1 4 
                      : makeParticle (x,y) (-14, 0) 1 4
                      : makeParticle (x,y) (14, 0) 1 4
                      : makeParticle (x,y) (10, 10) 1 4
                      : makeParticle (x,y) (-10, 10) 1 4
                      : makeParticle (x,y) (-10, -10) 1 4
                      : makeParticle (x,y) (10, -10) 1 4 : []

updateParticles :: Float -> GameState -> [Explosion]
updateParticles secs gs | all (==False) (map fst (collisions)) = updateParticles' secs (explosions gs)
                       | otherwise = updateParticles' secs ((map createExplosion (map snd (collisions))) ++ (explosions gs))
  where
    collisions = [x | x <- map (\x -> asteroidAllBullet x (bullets gs)) (asteroids gs), fst x == True]


updateParticles' :: Float -> [Explosion] -> [Explosion]
updateParticles' _ [] = []
updateParticles' t (xs:xss) = moveParticles t xs : updateParticles' t xss

moveParticles :: Float -> Explosion -> Explosion
moveParticles _ [] = []
moveParticles t ((Particle(Collide(x,y)r)(vx,vy) age exp):xs) 
  | age > exp = moveParticles t xs
  | otherwise = Particle (Collide(x+vx,y+vy)r) (vx*0.9,vy*0.9) (age+t) exp : moveParticles t xs

inGameWindow :: (Float,Float) -> (Float,Float) -> Bool
inGameWindow (x,y) (min,max) = x >= min && x <= max && y >= min && y <= max 

-- file functions
saveScore :: GameState -> IO ()
saveScore gamestate = do
  file     <- openFile "app/score.txt" ReadMode
  contents <- hGetLine file
  let highscore = parsingStringToInt contents
  hClose file
  file     <- openFile "app/score.txt" WriteMode
  let newHighscores    = concat ((map (\x -> show x ++ " ")) ((reverse.sort) (score gamestate:highscore)))
  hPutStr file newHighscores
  hClose file

topScores :: IO([Int])
topScores = do 
  file       <- openFile "app/score.txt" ReadMode
  highscores <- hGetLine file
  let scores = parsingStringToInt highscores
  hClose file
  return $ (take 5 scores)

parsingStringToInt :: String -> [Int]
parsingStringToInt [] = []
parsingStringToInt string = map read (words string)

-- helper functions
vectorL :: Velocity -> Float
vectorL (x, y) = sqrt(x^2 + y^2)

distance :: Location -> Location -> Float
distance (a,b) (m,n) = vectorL (a-m, b-n)

unitV :: Velocity -> Velocity
unitV v@(vX, vY) = (vX / length, vY / length)
  where 
    length = vectorL v

