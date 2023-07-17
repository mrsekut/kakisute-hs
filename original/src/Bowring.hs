{-# LANGUAGE TupleSections #-}
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
  = Open (ValidRoll, ValidRoll)
  | Spare ValidRoll
  | Strike
  deriving (Show)

data LastFrame
  = Two (ValidRoll, ValidRoll)
  | Thr (ValidRoll, ValidRoll, ValidRoll)
  deriving (Show)



-- TODO: clean
score :: [Roll] -> Either BowlingError Int
score rolls = do
  let a = indexedFoldM roll [] rolls
  b <- reverse <$> a
  c <- mapM unwrapPendingFrame b
  d <- validateFrameCount c
  pure $ framesScore d

  where
    unwrapPendingFrame :: PendingFrame -> Either BowlingError Frame
    unwrapPendingFrame (Done a) = pure a
    unwrapPendingFrame (Pend _) = Left IncompleteGame



-- TODO: やっぱこれが微妙なんだよなあ..
-- TODO: 書き換え案1: Stateモナド
data PendingFrame
  = Done Frame
  | Pend [ValidRoll] -- TODO: ここも厳密でない。Normalだと0/1要素で、ラストだけ1/2
  deriving (Show)

type Index = Int

-- NOTE: 先頭に追加して、最後にreverseする
-- NOTE: 最後までCommonでいっちゃう
-- TODO: clean: 分岐が多すぎ
roll :: [PendingFrame] -> (Index, Roll) -> Either BowlingError [PendingFrame]
roll [] cur  = (:[]) . mkFrame1 <$> mkValidRoll1 cur
roll ps@(prev:rest) (idx,roll)
  | length ps == 9 && isDone prev = rollForLast -- 10 frame目の1投目
  | length ps == 10               = rollForLast' prev
  | otherwise                     = rollForNormal prev

  where
    isDone (Done _) = True
    isDone _        = False

    -- TODO: clean
    rollForNormal :: PendingFrame -> Either BowlingError [PendingFrame]
    rollForNormal (Done _)     = (:ps) . mkFrame1 <$> mkValidRoll1 (idx,roll)
    rollForNormal (Pend (p:_)) = do
      (r1,r2) <- mkValidRoll2 p (idx,roll)
      pure $ (:rest) (mkFrame2 r1 r2)


-- TODO: type
    rollForLast = (\a -> Pend [a] : ps) <$> mkValidRoll1 (idx,roll)

-- TODO: type
    rollForLast' (Done _) = Left $ InvalidRoll idx roll -- prevが0 0 0
    rollForLast' (Pend [p]) = if unValidRoll p + roll < 10 -- TODO: unwrapしていいの？
        then do
          -- 10frameの2投目 2投で終了
          r <- mkValidRoll1 (idx,roll)
          pure $ Done (Last $ Two ( p,r )) : rest
        else do
          -- 10frameの2投目 3投目がある
          r <- mkValidRoll1 (idx,roll)
          pure $ Pend [p,r] : rest
    rollForLast' (Pend [p,q]) = do
          -- 10frame目 3投で終了
          (r1,r2,r3) <- mkValidRoll3 p q (idx,roll)
          pure $ Done (Last $ Thr ( r1,r2,r3 )) : rest








-- TODO: Done/Pend知ってるのがなあ.. あとは、Lastの時のロジックも結果的に含む?(分けるかもしれんが)
-- TODO: こんなunwrapしまくっていいの？
mkFrame1 :: ValidRoll -> PendingFrame
mkFrame1 r1
  | unValidRoll r1 == 10           = Done (Normal Strike)
  | 0 <= unValidRoll r1 && unValidRoll r1 < 10 = Pend [r1]


-- TODO: Done/Pend知ってるのがなあ.. あとは、Lastの時のロジックも結果的に含む?(分けるかもしれんが)
-- TODO: こんなunwrapしまくっていいの？
mkFrame2 :: ValidRoll -> ValidRoll -> PendingFrame
mkFrame2 r1 r2
  | unValidRoll r1 + unValidRoll r2 == 10 = Done $ Normal $ Spare r1
  | unValidRoll r1 + unValidRoll r2 < 10  = Done $ Normal $ Open (r1, r2)




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
    frameScore :: [Frame] -> Int
    frameScore ((Normal (Open (r1, r2))):_) = unValidRoll r1 + unValidRoll r2
    frameScore ((Normal (Spare _)):xs)      = 10 + getNext1 xs
    frameScore ((Normal Strike):xs)         = 10 + getNext2 xs
    frameScore (Last (Two (r1,r2)):_)       = unValidRoll r1 + unValidRoll r2
    frameScore ((Last (Thr (r1,r2,r3))):_)  = unValidRoll r1 + unValidRoll r2 + unValidRoll r3


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
    get (Normal (Open (n1,n2))) = unValidRoll n1 + unValidRoll n2
    get (Normal (Spare _))      = 10
    get (Normal Strike)         = 10 + getNext1 xs
    get (Last (Two (n1,n2)))    = unValidRoll n1 + unValidRoll n2
    get (Last (Thr (n1,n2,_)))  = unValidRoll n1 + unValidRoll n2



--
-- Valid Roll
--

-- 0~10
newtype ValidRoll = ValidRoll_ Int deriving (Show, Eq)


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

