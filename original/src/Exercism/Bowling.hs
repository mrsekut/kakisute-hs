{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

-- https://scrapbox.io/mrsekut-p/Bowling_(exercism)
module Exercism.Bowling (score, BowlingError(..)) where
import           Control.Applicative (Applicative (..))
import           Control.Monad
import           Control.Monad.State (MonadState (get, put), MonadTrans (lift),
                                      State, StateT, modify, runState,
                                      runStateT)



data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


data Frame
  = Normal NormalFrame
  | Last LastFrame
  deriving (Show)

data NormalFrame
  = Open (ValidRoll, ValidRoll)
  | Spare ValidRoll
  | Strike
  deriving (Show)

data LastFrame
  = Two (ValidRoll, ValidRoll)
  | Thr (ValidRoll, ValidRoll, ValidRoll)
  deriving (Show)



-- >>> score [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
-- Left (InvalidRoll {rollIndex = 20, rollValue = 0})
score :: [Roll] -> Either BowlingError Int
score rolls = do
  (frames,_) <- runStateT (indexedFoldM roll [] rolls) Done
  let a = reverse frames
  b <- validateFrameCount a
  pure $ framesScore b


framesScore :: [Frame] -> Int
framesScore []     = 0
framesScore (x:xs) = frameScore (x:xs) + framesScore xs
  where
    frameScore :: [Frame] -> Int
    frameScore ((Normal (Open (r1, r2))):_) = unValidRoll $ r1 + r2
    frameScore ((Normal (Spare _)):xs)      = 10 + getNext1 xs
    frameScore ((Normal Strike):xs)         = 10 + getNext2 xs
    frameScore (Last (Two (r1,r2)):_)       = unValidRoll $ r1 + r2
    frameScore ((Last (Thr (r1,r2,r3))):_)  = unValidRoll $ r1 + r2 + r3


getNext1 :: [Frame] -> Int
getNext1 [] = 0
getNext1 (x:_) = get x
  where
    get :: Frame -> Int
    get (Normal (Open (n1,_))) = unValidRoll n1
    get (Normal (Spare n1))    = unValidRoll n1
    get (Normal Strike)        = 10
    get (Last (Two (n1,_)))    = unValidRoll n1
    get (Last (Thr (n1,_,_)))  = unValidRoll n1


getNext2 :: [Frame] -> Int
getNext2 []     = 0
getNext2 (x:xs) = get x
  where
    get :: Frame -> Int
    get (Normal (Open (n1,n2))) = unValidRoll $ n1 + n2
    get (Normal (Spare _))      = 10
    get (Normal Strike)         = 10 + getNext1 xs
    get (Last (Two (n1,n2)))    = unValidRoll $ n1 + n2
    get (Last (Thr (n1,n2,_)))  = unValidRoll $ n1 + n2



--
-- Roll
--

type Roll = Int
type Index = Int

type WithPendingFrame a = StateT PendingFrame (Either BowlingError) a

data PendingFrame
  = Done
  | Pend [ValidRoll] -- TODO: ここも厳密でない。Normalだと0/1要素で、ラストだけ1/2
  deriving (Show)


-- 先頭に追加して、最後にreverseする
roll :: [Frame] -> (Index,Roll) -> WithPendingFrame [Frame]
roll fs cur
  | length fs <= 8  = do
      p <- get
      rollForNormal p
  | length fs == 9 = do
      p <- get
      rollFor10 p cur
  | otherwise = lift $ Left (uncurry InvalidRoll cur)

  where
    rollForNormal :: PendingFrame -> WithPendingFrame [Frame]
    rollForNormal Done = do
      r1 <- lift $ mkValidRoll1 cur
      frame <- putPendingFrame1 r1
      pure $ maybe fs (:fs) frame
    rollForNormal (Pend [p]) = do
      (r1, r2) <- lift $ mkValidRoll2 p cur
      frame <- mkPendingFrame2 r1 r2
      pure $ frame : fs


    -- TODO: mkPendingFrame, 関数分割
    rollFor10 :: PendingFrame -> (Index, Roll) -> WithPendingFrame [Frame]
    -- rollFor10 Done (idx,roll) = lift . Left $ InvalidRoll idx roll -- prevが0 0 0
    rollFor10 Done cur = do
      r1 <- lift $ mkValidRoll1 cur
      put $ Pend [r1]
      pure fs
    rollFor10 (Pend [r1]) (idx,roll)
      | unValidRoll r1 + roll < 10 = do
          -- 10frameの2投目 2投で終了
          r2 <- lift $ mkValidRoll1 (idx,roll)
          put Done
          pure $ Last (Two (r1,r2)) : fs
      | otherwise = do
          -- 10frameの2投目 3投目がある
          r2 <- lift $ mkValidRoll1 (idx,roll)
          put $ Pend [r1,r2]
          pure fs
    rollFor10 (Pend [p1,p2]) cur = do
      (r1,r2,r3) <- lift $ mkValidRoll3 p1 p2 cur
      put Done
      pure $ Last (Thr (r1,r2,r3)) : fs


putPendingFrame1 :: ValidRoll -> WithPendingFrame (Maybe Frame)
putPendingFrame1 r1
  | unValidRoll r1 == 10 = do
      put Done
      pure . Just $ Normal Strike
  | otherwise            = do
      put (Pend [r1])
      pure Nothing


mkPendingFrame2 :: ValidRoll -> ValidRoll -> WithPendingFrame Frame
mkPendingFrame2 r1 r2
  | unValidRoll (r1 + r2) == 10 = do
      put Done
      pure $ Normal (Spare r1)
  | otherwise                   = do
      put Done
      pure $ Normal (Open (r1, r2))



--
-- Valid Roll
--

-- 0~10
newtype ValidRoll = ValidRoll_ Int deriving (Show, Eq, Num)


mkValidRoll1 :: (Index, Roll) -> Either BowlingError ValidRoll
mkValidRoll1 (idx,r1)
  | isValidRoll1 r1 = pure $ ValidRoll_ r1
  | otherwise       = Left $ InvalidRoll idx r1


mkValidRoll2 :: ValidRoll -> (Index, Roll) -> Either BowlingError (ValidRoll,ValidRoll)
mkValidRoll2 r1 (idx,r2)
  | isValidRoll2 r1 r2 = (r1,) <$> mkValidRoll1 (idx,r2)
  | otherwise          = Left $ InvalidRoll idx r2


mkValidRoll3 :: ValidRoll -> ValidRoll -> (Index, Roll) -> Either BowlingError (ValidRoll,ValidRoll,ValidRoll)
mkValidRoll3 (ValidRoll_ r1') (ValidRoll_ r2') roll@(idx, r3)
  -- strike*2&open, strike*3
  | r1' == 10 && r2' == 10 = (,,) (ValidRoll_ r1') (ValidRoll_ r2') <$> mkValidRoll1 roll

  -- strike&open, strike&spare
  | r1' == 10              = (\(a2,a3) -> (ValidRoll_ r1',a2,a3)) <$> mkValidRoll2 (ValidRoll_ r2') roll

  -- spare&open, spare&strike
  | r1' + r2' == 10        = (,,) (ValidRoll_ r1') (ValidRoll_ r2') <$> mkValidRoll1 roll

  | otherwise              = Left $ InvalidRoll idx r3



unValidRoll :: ValidRoll -> Int
unValidRoll (ValidRoll_ r) = r



--
-- Validations
--

isValidRoll1 :: Roll -> Bool
isValidRoll1 r1 = 0 <= r1 && r1 <= 10


isValidRoll2 :: ValidRoll -> Roll -> Bool
isValidRoll2 (ValidRoll_ r1) r2 = r1 + r2 <= 10


validateFrameCount :: [Frame] -> Either BowlingError [Frame]
validateFrameCount fs
  | length fs >= 10 = pure fs
  | otherwise       = Left IncompleteGame




--
-- Utilities
--

indexedFoldM :: Monad m => (a -> (Int, b) -> m a) -> a -> [b] -> m a
indexedFoldM f acc xs = foldM f acc (zip [0..] xs)
