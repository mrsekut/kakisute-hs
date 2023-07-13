module Bowling (score, BowlingError(..)) where
import           Control.Applicative (Applicative (..))
import           Control.Monad



data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


type Roll = Int

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
    pure $ framesScore d
-- TODO: clean


-- TODO: やっぱこれが微妙なんだよなあ..
-- TODO: 書き換え案1: Stateモナド
data PendingFrame
  = Done CommonFrame
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


--
--
--
--
--
--
--
--
--
--
--

framesScore :: [Frame] -> Int
framesScore []     = 0
framesScore (x:xs) = frameScore (x:xs) + framesScore xs
  where
    frameScore ((Normal (Open (r1, r2))):_) = r1 + r2
    frameScore ((Normal (Spare _)):xs)      = 10 + getNext1 xs
    frameScore ((Normal Strike):xs)         = 10 + getNext2 xs
    frameScore (Last (Two (r1,r2)):_)       = r1 + r2
    frameScore ((Last (Thr (r1,r2,r3))):_)  = r1 + r2 + r3


getNext1 :: [Frame] -> Int
getNext1 [] = 0
getNext1 (x:_) = get x
  where
    get :: Frame -> Int
    get (Normal (Open (n1,_))) = n1
    get (Normal (Spare n1))    = n1
    get (Normal Strike)        = 10
    get (Last (Two (n1,_)))    = n1
    get (Last (Thr (n1,_,_)))  = n1


getNext2 :: [Frame] -> Int
getNext2 []     = 0
getNext2 (x:xs) = get x
  where
    get :: Frame -> Int
    get (Normal (Open (n1,n2))) = n1 + n2
    get (Normal (Spare _))      = 10
    get (Normal Strike)         = 10 + getNext1 xs
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
