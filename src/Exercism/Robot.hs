module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

newtype Robot = Robot (Bearing, (Integer, Integer))

bearing :: Robot -> Bearing
bearing (Robot (b,_)) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot (_,c)) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot (direction, coordinates)

move :: Robot -> String -> Robot
move = foldl run
  where
    run r 'R' = turn right r
    run r 'L' = turn left r
    run r 'A' = advance r

    turn :: (Bearing -> Bearing) -> Robot -> Robot
    turn f (Robot (b,c)) = Robot (f b, c)

    advance :: Robot -> Robot
    advance (Robot (b,(x,y))) = case b of
      North -> Robot (b, (x,y+1))
      East  -> Robot (b, (x+1,y))
      South -> Robot (b, (x,y-1))
      West  -> Robot (b, (x-1,y))

    right :: Bearing -> Bearing
    right North = East
    right East  = South
    right South = West
    right West  = North

    left :: Bearing -> Bearing
    left East  = North
    left South = East
    left West  = South
    left North = West

