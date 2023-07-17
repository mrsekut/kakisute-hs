{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Bowling (score, BowlingError(..)) where
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



-- >>> score [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]
-- Right ([Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6)),Normal (Open (ValidRoll_ 3,ValidRoll_ 6))],Done)
score rolls = do
  pendingFrames <- runStateT (indexedFoldM roll [] rolls) Done
  -- pendingFrames <- runStateT (reverse <$> indexedFoldM roll [] rolls) Done
  pure pendingFrames
  -- undefined
  -- pure pendingFrames

  -- frames <- pendingFrames2frames pendingFrames
  -- pure $ framesScore frames
-- score :: [Roll] -> Either BowlingError Int
-- score rolls = undefined



pendingFrames2frames :: [PendingFrame] -> Either BowlingError [Frame]
pendingFrames2frames ps = mapM unwrapPendingFrame ps >>= validateFrameCount
  where
    unwrapPendingFrame :: PendingFrame -> Either BowlingError Frame
    unwrapPendingFrame = undefined
    -- unwrapPendingFrame (Done a) = pure a
    -- unwrapPendingFrame (Pend _) = Left IncompleteGame


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
-- TODO: 分岐
roll :: [Frame] -> (Index,Roll) -> WithPendingFrame [Frame]
roll ps cur
  -- | length ps == 9 && isDone prev = rollFor10First cur -- 10 frame目の1投目
  -- | length ps == 10               = rollFor10 prev cur
  | False = undefined
  | otherwise                     = do
      p <- get
      rollForNormal p cur

  where
    -- isDone Done = True
    -- isDone _    = False

    rollForNormal :: PendingFrame -> (Index, Roll) -> WithPendingFrame [Frame]
    rollForNormal Done cur = do
      r1 <- lift $ mkValidRoll1 cur
      frame <- putPendingFrame1 r1
      pure $ maybe ps (:ps) frame
    rollForNormal (Pend [p]) cur = do
      (r1, r2) <- lift $ mkValidRoll2 p cur
      frame <- mkPendingFrame2 r1 r2
      pure $ frame : ps

    -- rollForNormal :: PendingFrame -> (Index, Roll) -> Either BowlingError [PendingFrame]
    -- rollForNormal = undefined
    -- rollForNormal Done cur     = do
    --   r1 <- mkValidRoll1 cur
    --   pure $ mkPendingFrame1 r1 : ps
    -- rollForNormal (Pend (p:_)) cur = do
    --   (r1,r2) <- mkValidRoll2 p cur
    --   pure $ mkPendingFrame2 r1 r2 : rest


    -- TODO: mkPendingFrame
    rollFor10First :: (Index, Roll) -> Either BowlingError [PendingFrame]
    rollFor10First = undefined
    -- rollFor10First cur = do
    --   r1 <- mkValidRoll1 cur
    --   pure $ Pend [r1] : ps

    -- TODO: mkPendingFrame
    rollFor10 :: PendingFrame -> (Index, Roll) -> Either BowlingError [PendingFrame]
    rollFor10 = undefined
    -- rollFor10 Done (idx,roll)  = Left $ InvalidRoll idx roll -- prevが0 0 0
    -- rollFor10 (Pend [r1]) (idx,roll)
    --   | unValidRoll r1 + roll < 10 = do
    --       -- 10frameの2投目 2投で終了
    --       r2 <- mkValidRoll1 (idx,roll)
    --       pure $ Done (Last $ Two (r1,r2)) : rest
    --   | otherwise = do
    --       -- 10frameの2投目 3投目がある
    --       r2 <- mkValidRoll1 (idx,roll)
    --       pure $ Pend [r1,r2] : rest
    -- rollFor10 (Pend [p,q]) cur = do
    --   -- 10frame目 3投で終了
    --   (r1,r2,r3) <- mkValidRoll3 p q cur
    --   pure $ Done (Last $ Thr (r1,r2,r3)) : rest


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
