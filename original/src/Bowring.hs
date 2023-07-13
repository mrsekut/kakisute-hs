module Bowling (score, BowlingError(..)) where
import           Control.Applicative (Applicative (..))
import           Control.Monad



data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


type Roll = Int
type NextRoll = Roll

data Frame
  = Normal NormalFrame
  | Last LastFrame
  deriving (Show)

data NormalFrame
  = Open (Roll, Roll)
  | Spare Roll
  | Strike
  deriving (Show)

data LastFrame
  = Two (Roll, Roll)
  | Thr (Roll, Roll, Roll)
  deriving (Show)
-- TODO: まあ、[Roll]だけでも良さげ..



-- >>> score [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]
-- Right 17
score :: [Roll] -> Either BowlingError Int
score rolls = do
    let a = indexedFoldM roll [] rolls
    b <- reverse <$> a
    c <- mapM unwrapPendingFrame b
    d <- validateFrameCount c
    pure $ score' d
-- TODO: clean


-- TODO: やっぱこれが微妙なんだよなあ..
-- TODO: 書き換え案1: Stateモナド
data PendingFrame
  = Done CommonFrame -- TODO: ここのFrame参照されてない？
  | Pend [Roll] -- TODO: ここも厳密でない。Normalだと0/1要素で、ラストだけ1/2
  deriving (Show)

data CommonFrame
  = OpenC (Roll, Roll)
  | SpareC Roll
  | StrikeC
  | LastC [Roll] -- 2 or 3 elements
  deriving (Show)

type Index = Int

-- NOTE: 先頭に追加して、最後にreverseする
-- NOTE: 最後までCommonでいっちゃう
-- TODO: clean: 分岐が多すぎ
roll :: [PendingFrame] -> (Index, Roll) -> Either BowlingError [PendingFrame]
roll [] cur  = (:[]) <$> mkFrame1 cur
roll ps@(last:tail) (idx,roll)
  | length ps == 9 && isDone last = do
      -- 10 frame目の1投目
      r <- validateRoll1 (idx,roll)
      pure $ Pend [r] : ps
  | length ps == 10 = case last of
      Done _ -> Left $ InvalidRoll idx roll -- lastが0 0 0
      Pend [p]   -> if p + roll < 10
        then do
          -- 10frameの2投目 2投で終了
          r <- validateRoll1 (idx,roll)
          pure $ Done (LastC [p,r]) : tail
        else do
          -- 10frameの2投目 3投目がある
          r <- validateRoll1 (idx,roll)
          pure $ Pend [p,r] : tail
      Pend [p,q] -> do
          -- 10frame目 3投で終了
          r <- validateRoll1 (idx,roll)
          rs <- validateRoll3 p q (idx,r)
          pure $ Done (LastC rs) : tail

    --  Pend [p,q] -> pure $ Done (LastC [p,q,roll]) : tail
  | otherwise = do
    -- TODO: clean
    case last of
      Done _     -> (:ps) <$> mkFrame1 (idx,roll)
      Pend (p:_) -> (:tail) <$> mkFrame2 p (idx,roll)

  where
    isDone (Done _) = True
    isDone _        = False


-- TODO: where?, validateRoll1に置き換え
mkFrame1 :: (Index, Roll) -> Either BowlingError PendingFrame
mkFrame1 (idx,r1)
  | r1 == 10           = pure $ Done StrikeC
  | 0 <= r1 && r1 < 10 = pure $ Pend [r1]
  | otherwise          = Left $ InvalidRoll idx r1



-- TODO: where? validateRoll2を使って書けない？
mkFrame2 :: Roll -> (Index, Roll) -> Either BowlingError PendingFrame
mkFrame2 r1 (idx,r2)
  | r1 + r2 == 10 = pure $ Done $ SpareC r1
  | r1 + r2 < 10  = pure $ Done $ OpenC (r1, r2)
  | otherwise     = Left $ InvalidRoll idx r2




-- TODO: Common to Frame (Doneを剥がしたり、Lastを作ったり)
unwrapPendingFrame :: PendingFrame -> Either BowlingError Frame
unwrapPendingFrame (Done (OpenC (r1,r2)))    = pure $ Normal $ Open (r1,r2)
unwrapPendingFrame (Done (SpareC r))         = pure $ Normal $ Spare r
unwrapPendingFrame (Done StrikeC)            = pure $ Normal Strike
unwrapPendingFrame (Done (LastC [r1,r2]))    = pure $ Last $ Two (r1,r2)
unwrapPendingFrame (Done (LastC [r1,r2,r3])) = pure $ Last $ Thr (r1,r2,r3)
unwrapPendingFrame (Pend _)                  = Left IncompleteGame


-- TODO: name
score' :: [Frame] -> Int
score' []                            = 0
score' ((Normal (Open (r1, r2))):fs) = r1 + r2 + score' fs
score' ((Normal (Spare r)):fs)       = 10 + getNext1 fs + score' fs
score' ((Normal Strike):fs)          = 10 + getNext2 fs + score' fs
score' (Last (Two (r1,r2)):fs)       = r1 + r2 + score' fs
score' ((Last (Thr (r1,r2,r3))):fs)  = r1 + r2 + r3 + score' fs


getNext1 :: [Frame] -> Int
getNext1 [] = 0
getNext1 (f:_) = get f
  where
    get :: Frame -> Int
    get (Normal (Open (n1,_))) = n1
    get (Normal (Spare n1))    = n1
    get (Normal Strike)        = 10
    get (Last (Two (n1,_)))    = n1
    get (Last (Thr (n1,_,_)))  = n1


getNext2 :: [Frame] -> Int
getNext2 (f:fs) = get f
  where
    get :: Frame -> Int
    get (Normal (Open (n1,n2))) = n1 + n2
    get (Normal (Spare n1))     = 10
    get (Normal Strike)         = 10 + getNext1 fs
    get (Last (Two (n1,n2)))    = n1 + n2
    get (Last (Thr (n1,n2,_)))  = n1 + n2



--
-- Validations
--

isValidRoll1 :: Roll -> Bool
isValidRoll1 r1 = 0 <= r1 && r1 <= 10


isValidRoll2 :: Roll -> Roll -> Bool
isValidRoll2 r1 r2 = r1 + r2 <= 10


validateRoll1 :: (Index, Roll) -> Either BowlingError Roll
validateRoll1 (idx,r1)
  | isValidRoll1 r1 = pure r1
  | otherwise       = Left $ InvalidRoll idx r1


validateRoll2 :: Roll -> (Index, Roll) -> Either BowlingError [Roll]
validateRoll2 r1 (idx,r2)
  | isValidRoll2 r1 r2 = (\r -> [r1,r]) <$> validateRoll1 (idx,r2)
  | otherwise          = Left $ InvalidRoll idx r2


validateRoll3 :: Roll -> Roll -> (Index, Roll) -> Either BowlingError [Roll]
validateRoll3 r1 r2 (idx,r3)
  | (r1,r2) == (10,10) = (\r -> [r1,r2,r]) <$> validateRoll1 (idx, r3)  -- strike*2&open, strike*3
  | r1 == 10           = (r1:) <$> validateRoll2 r2 (idx,r3)            -- strike&open, strike&spare
  | r1 + r2 == 10      = (\r -> [r1,r2,r]) <$> validateRoll1 (idx, r3)  -- spare&open, spare&strike
  | otherwise          = Left $ InvalidRoll idx r3


validateFrameCount :: [Frame] -> Either BowlingError [Frame]
validateFrameCount fs
  | length fs >= 10 = pure fs
  | otherwise       = Left IncompleteGame




--
-- Utilities
--

indexedFoldM :: Monad m => (a -> (Int, b) -> m a) -> a -> [b] -> m a
indexedFoldM f acc xs = foldM f acc (zip [0..] xs)







-- cases = [ Case { description = "should be able to score a game with all zeros"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 0
--                }
--         , Case { description = "should be able to score a game with no strikes or spares"
--                , rolls       = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]
--                , expected    = Right 90
--                }
--         , Case { description = "a spare followed by zeros is worth ten points"
--                , rolls       = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 10
--                }
--         , Case { description = "points scored in the roll after a spare are counted twice"
--                , rolls       = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 16
--                }
--         , Case { description = "consecutive spares each get a one roll bonus"
--                , rolls       = [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 31
--                }
--         , Case { description = "a spare in the last frame gets a one roll bonus that is counted once"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]
--                , expected    = Right 17
--                }
--         , Case { description = "a strike earns ten points in a frame with a single roll"
--                , rolls       = [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 10
--                }
--         , Case { description = "points scored in the two rolls after a strike are counted twice as a bonus"
--                , rolls       = [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 26
--                }
--         , Case { description = "consecutive strikes each get the two roll bonus"
--                , rolls       = [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Right 81
--                }
--         , Case { description = "a strike in the last frame gets a two roll bonus that is counted once"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1]
--                , expected    = Right 18
--                }
--         , Case { description = "rolling a spare with the two roll bonus does not get a bonus roll"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3]
--                , expected    = Right 20
--                }
--         , Case { description = "strikes with the two roll bonus do not get bonus rolls"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10]
--                , expected    = Right 30
--                }
--         , Case { description = "a strike with the one roll bonus after a spare in the last frame does not get a bonus"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10]
--                , expected    = Right 20
--                }
--         , Case { description = "all strikes is a perfect game"
--                , rolls       = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
--                , expected    = Right 300
--                }
--         , Case { description = "rolls cannot score negative points"
--                , rolls       = [-1]
--                , expected    = Left $ InvalidRoll 0 (-1)
--                }
--         , Case { description = "a roll cannot score more than 10 points"
--                , rolls       = [11]
--                , expected    = Left $ InvalidRoll 0 11
--                }
--         , Case { description = "two rolls in a frame cannot score more than 10 points"
--                , rolls       = [5, 6]
--                , expected    = Left $ InvalidRoll 1 6
--                }
--         , Case { description = "bonus roll after a strike in the last frame cannot score more than 10 points"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11]
--                , expected    = Left $ InvalidRoll 19 11
--                }
--         , Case { description = "two bonus rolls after a strike in the last frame cannot score more than 10 points"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5, 6]
--                , expected    = Left $ InvalidRoll 20 6
--                }
--         , Case { description = "two bonus rolls after a strike in the last frame can score more than 10 points if one is a strike"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6]
--                , expected    = Right 26
--                }
--         , Case { description = "the second bonus rolls after a strike in the last frame cannot be a strike if the first one is not a strike"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6, 10]
--                , expected    = Left $ InvalidRoll 20 10
--                }
--         , Case { description = "second bonus roll after a strike in the last frame cannot score more than 10 points"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 11]
--                , expected    = Left $ InvalidRoll 20 11
--                }
--         , Case { description = "an unstarted game cannot be scored"
--                , rolls       = []
--                , expected    = Left IncompleteGame
--                }
--         , Case { description = "an incomplete game cannot be scored"
--                , rolls       = [0, 0]
--                , expected    = Left IncompleteGame
--                }
--         , Case { description = "cannot roll if game already has ten frames"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--                , expected    = Left $ InvalidRoll 20 0
--                }
--         , Case { description = "bonus rolls for a strike in the last frame must be rolled before score can be calculated"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
--                , expected    = Left IncompleteGame
--                }
--         , Case { description = "both bonus rolls for a strike in the last frame must be rolled before score can be calculated"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]
--                , expected    = Left IncompleteGame
--                }
--         , Case { description = "bonus roll for a spare in the last frame must be rolled before score can be calculated"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3]
--                , expected    = Left IncompleteGame
--                }
--         , Case { description = "cannot roll after bonus roll for spare"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 2, 2]
--                , expected    = Left $ InvalidRoll 21 2
--                }
--         , Case { description = "cannot roll after bonus rolls for strike"
--                , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 2, 2]
--                , expected    = Left $ InvalidRoll 21 2
--                }
