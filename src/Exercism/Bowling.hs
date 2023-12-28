{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Exercism.Bowling (score, BowlingError(..)) where
import           Control.Applicative  (Applicative (..))
import           Control.Monad
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (MonadState (get, put), MonadTrans (lift),
                                       StateT, runStateT)



data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


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



score :: [Roll] -> Either BowlingError Int
score = undefined
-- score rolls = do
--   (frames',_) <- runStateT (indexedFoldM roll [] rolls) Done
--   frames <- validateFrameCount frames'
--   pure . framesScore . reverse $ frames


-- framesScore :: [Frame] -> Int
-- framesScore []     = 0
-- framesScore (x:xs) = frameScore (x:xs) + framesScore xs
--   where
--     frameScore :: [Frame] -> Int
--     frameScore ((Normal (Open (r1, r2))):_) = unValidRoll $ r1 + r2
--     frameScore ((Normal (Spare _)):xs)      = 10 + getNext1 xs
--     frameScore ((Normal Strike):xs)         = 10 + getNext2 xs
--     frameScore (Last (Two (r1,r2)):_)       = unValidRoll $ r1 + r2
--     frameScore ((Last (Thr (r1,r2,r3))):_)  = unValidRoll $ r1 + r2 + r3


-- getNext1 :: [Frame] -> Int
-- getNext1 [] = 0
-- getNext1 (x:_) = get x
--   where
--     get :: Frame -> Int
--     get (Normal (Open (n1,_))) = unValidRoll n1
--     get (Normal (Spare n1))    = unValidRoll n1
--     get (Normal Strike)        = 10
--     get (Last (Two (n1,_)))    = unValidRoll n1
--     get (Last (Thr (n1,_,_)))  = unValidRoll n1


-- getNext2 :: [Frame] -> Int
-- getNext2 []     = 0
-- getNext2 (x:xs) = get x
--   where
--     get :: Frame -> Int
--     get (Normal (Open (n1,n2))) = unValidRoll $ n1 + n2
--     get (Normal (Spare _))      = 10
--     get (Normal Strike)         = 10 + getNext1 xs
--     get (Last (Two (n1,n2)))    = unValidRoll $ n1 + n2
--     get (Last (Thr (n1,n2,_)))  = unValidRoll $ n1 + n2



--
-- Roll
--

type Roll = Int
type Index = Int

-- type WithPendingFrame a = StateT PendingFrame (Either BowlingError) a

-- data PendingFrame
--   = Done
--   | Pend [ValidRoll] -- 0 or 1 in Normal, 1 or 2 in Last
--   deriving (Show)


roll2 :: MonadError BowlingError m => [(Index,Roll)] -> m [Frame]
roll2 rolls = do
  farmes <- replicateM 9 (normalFrame rolls)
  lastFrame <- rollForLast rolls
  pure $ map Normal farmes ++ [Last lastFrame]


normalFrame :: MonadError BowlingError m => [(Int, Roll)] -> m NormalFrame
normalFrame ((_,10):rs)      = pure Strike
normalFrame ((_,x):(i,y):rs)
  | x + y < 10  = pure $ Open (x, y)
  | x + y == 10 = pure $ Spare x
  | otherwise   = throwError $ InvalidRoll i y


rollForLast :: MonadError BowlingError m => [(Int, Roll)] -> m LastFrame
rollForLast ((_,x):(_,y):(_,z):rs) = pure $ Thr (x,y,z)
rollForLast ((_,x):(_,y):rs)       = pure $ Two (x,y)
rollForLast _                      = throwError IncompleteGame


-- 先頭に追加して、最後にreverseする
-- roll :: [Frame] -> (Index,Roll) -> WithPendingFrame [Frame]
-- roll fs (idx,roll)
--   | length fs <= 8  = do
--       p <- get
--       rollForNormal p (idx,roll)
--   | length fs == 9 = do
--       p <- get
--       rollForLast p (idx,roll)
--   | otherwise = lift $ Left (InvalidRoll idx roll)

--   where
--     rollForNormal :: PendingFrame -> (Index,Roll) -> WithPendingFrame [Frame]
--     rollForNormal Done cur       = do
--       r1 <- lift $ mkValidRoll1 cur
--       if unValidRoll r1 == 10
--         then do
--           put Done
--           pure $ Normal Strike : fs
--         else do
--           put (Pend [r1])
--           pure fs
--     rollForNormal (Pend [p]) cur = do
--       (r1, r2) <- lift $ mkValidRoll2 p cur
--       if unValidRoll (r1 + r2) == 10
--         then do
--           put Done
--           pure $ Normal (Spare r1) : fs
--         else do
--           put Done
--           pure $ Normal (Open (r1, r2)) : fs


--     rollForLast :: PendingFrame -> (Index, Roll) -> WithPendingFrame [Frame]
--     rollForLast Done cur = do
--       r1 <- lift $ mkValidRoll1 cur
--       put $ Pend [r1]
--       pure fs
--     rollForLast (Pend [r1]) (idx,roll)
--       | unValidRoll r1 + roll < 10 = do
--           r2 <- lift $ mkValidRoll1 (idx,roll)
--           put Done
--           pure $ Last (Two (r1,r2)) : fs
--       | otherwise = do
--           r2 <- lift $ mkValidRoll1 (idx,roll)
--           put $ Pend [r1,r2]
--           pure fs
--     rollForLast (Pend [p1,p2]) cur = do
--       (r1,r2,r3) <- lift $ mkValidRoll3 p1 p2 cur
--       put Done
--       pure $ Last (Thr (r1,r2,r3)) : fs



-- --
-- -- Valid Roll
-- --

-- -- 0~10
-- newtype ValidRoll = ValidRoll_ Int deriving (Show, Eq, Num)


-- mkValidRoll1 :: (Index, Roll) -> Either BowlingError ValidRoll
-- mkValidRoll1 (idx,r1)
--   | isValidRoll1 r1 = pure $ ValidRoll_ r1
--   | otherwise       = Left $ InvalidRoll idx r1


-- mkValidRoll2 :: ValidRoll -> (Index, Roll) -> Either BowlingError (ValidRoll,ValidRoll)
-- mkValidRoll2 r1 (idx,r2)
--   | isValidRoll2 r1 r2 = (r1,) <$> mkValidRoll1 (idx,r2)
--   | otherwise          = Left $ InvalidRoll idx r2


-- mkValidRoll3 :: ValidRoll -> ValidRoll -> (Index, Roll) -> Either BowlingError (ValidRoll,ValidRoll,ValidRoll)
-- mkValidRoll3 (ValidRoll_ r1') (ValidRoll_ r2') roll@(idx, r3)
--   -- strike*2&open, strike*3
--   | r1' == 10 && r2' == 10 = (,,) (ValidRoll_ r1') (ValidRoll_ r2') <$> mkValidRoll1 roll

--   -- strike&open, strike&spare
--   | r1' == 10              = (\(a2,a3) -> (ValidRoll_ r1',a2,a3)) <$> mkValidRoll2 (ValidRoll_ r2') roll

--   -- spare&open, spare&strike
--   | r1' + r2' == 10        = (,,) (ValidRoll_ r1') (ValidRoll_ r2') <$> mkValidRoll1 roll

--   | otherwise              = Left $ InvalidRoll idx r3



-- unValidRoll :: ValidRoll -> Int
-- unValidRoll (ValidRoll_ r) = r



--
-- Validations
--

isValidRoll1 :: Roll -> Bool
isValidRoll1 r1 = 0 <= r1 && r1 <= 10


isValidRoll2 :: Roll -> Roll -> Bool
isValidRoll2 r1 r2 = r1 + r2 <= 10


validateFrameCount :: [Frame] -> Either BowlingError [Frame]
validateFrameCount fs
  | length fs >= 10 = pure fs
  | otherwise       = Left IncompleteGame




--
-- Utilities
--

indexedFoldM :: Monad m => (a -> (Int, b) -> m a) -> a -> [b] -> m a
indexedFoldM f acc xs = foldM f acc (zip [0..] xs)
