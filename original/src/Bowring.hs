module Bowling (score, BowlingError(..)) where
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


-- >>> score [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 5]
-- /Users/mrsekut/Desktop/dev/github.com/mrsekut/kakisute-hs/original/src/Bowring.hs:(93,1)-(95,67): Non-exhaustive patterns in function score'

score :: [Roll] -> Either BowlingError Int
-- TODO: clean
score rolls = d
  where
    a = indexedFoldM roll [] rolls
    b = reverse <$> a
    c = map unwrapPendingFrame <$> b
    d = score' <$> c


-- TODO: やっぱこれが微妙なんだよなあ..
-- TODO: 書き換え案1: Stateモナド
data PendingFrame
  = Done Frame -- TODO: ここのFrame参照されてない？
  | Pend [Roll]
  deriving (Show)

type Index = Int

-- NOTE: 先頭に追加して、最後にreverseする
roll :: [PendingFrame] -> (Index, Roll) -> Either BowlingError [PendingFrame]
roll [] cur  = (:[]) <$> mkFrame1 cur
roll ps@(last:tail) cur
  | length ps == 9 && isDone last = undefined -- 10frameの初回。pendに追加するだけ
  | length ps == 10 = undefined -- 10frameの2 or 3回目
  | otherwise = do
    -- TODO: clean
    a <- case last of
      Done _            -> mkFrame1 cur
      Pend (prevRoll:_) -> mkFrame2 prevRoll cur

    return $ case a of
      Done _ -> a:tail
      Pend _ -> a:ps



-- TODO: 知りすぎ？ それならwhere無いでもいいかも？
mkFrame1 :: (Index, Roll) -> Either BowlingError PendingFrame
mkFrame1 (idx,r1)
  | r1 == 10           = pure $ Done $ Normal Strike
  | 0 <= r1 && r1 < 10 = pure $ Pend [r1]
  | otherwise          = Left $ InvalidRoll idx r1


-- TODO: 知りすぎ？
mkFrame2 :: Roll -> (Index, Roll) -> Either BowlingError PendingFrame
mkFrame2 r1 (idx,r2)
  | r1 + r2 == 10 = pure $ Done $ Normal $ Spare r1
  | r1 + r2 < 10  = pure $ Done $ Normal $ Open (r1, r2)
  | otherwise     = Left $ InvalidRoll idx r2




unwrapPendingFrame :: PendingFrame -> Frame
unwrapPendingFrame (Done frame) = frame
unwrapPendingFrame (Pend _)     = error ""


-- TODO: name
score' :: [Frame] -> Int
score' []                            = 0
score' ((Normal (Open (r1, r2))):fs) = r1 + r2 + score' fs
score' ((Normal (Spare r)):fs)       = 10 + getNext1 fs + score' fs
score' ((Normal Strike):fs)          = 10 + getNext2 fs + score' fs
-- TODO: last


getNext1 :: [Frame] -> Int
getNext1 (f:_) = get f
  where
    get :: Frame -> Int
    get (Normal (Open (n1,_))) = n1
    get (Normal (Spare n1))    = n1
    get (Normal Strike)        = 10
    -- TODO: last


getNext2 :: [Frame] -> Int
getNext2 (f:fs) = get f
  where
    get :: Frame -> Int
    get (Normal (Open (n1,n2))) = n1 + n2
    get (Normal (Spare n1))     = 10
    get (Normal Strike)         = 10 + getNext1 fs
    -- TODO: last


-- utils
indexedFoldM :: Monad m => (a -> (Int, b) -> m a) -> a -> [b] -> m a
indexedFoldM f acc xs = foldM f acc (zip [0..] xs)



-- validates :: [Roll] -> Either BowlingError [Roll]
-- validates rs = step rs 0
--   where
--     step :: [Roll] -> Int -> Either BowlingError [Roll]
--     step [] _ = Right []
--     step (r:rs) idx
--       | r < 0 || r > 10 = Left $ InvalidRoll idx r
--       | otherwise       = (:) r <$> step rs (idx + 1)
--     -- step (rs, 10) = Right [Last rs]
--     -- step (r1:r2:r3:rs, idx)
--     --   | r1 == 10      = (:) (Normal $ Strike (r2, r3)) <$> step (r2:r3:rs, idx + 1)
--     --   | r1 + r2 == 10 = (:) (Normal $ Spare r3) <$> step (r3:rs, idx + 1)
--     --   | r1 + r2 < 10  = (:) (Normal $ Open (r1, r2)) <$> step (r3:rs, idx + 1)
--     --   | otherwise     = Left IncompleteGame -- TODO:


-- TODO: eitherやめたい？
-- makeFramePs :: [Roll] -> Either BowlingError [FrameP]
-- makeFramePs rolls = step (rolls, 1)
--   where
--     step ([], _)  = Left IncompleteGame
--     step (rs, 10) = Right [Last rs]
--     step (r1:r2:r3:rs, idx)
--       | r1 == 10      = (:) (Normal $ Strike (r2, r3)) <$> step (r2:r3:rs, idx + 1)
--       | r1 + r2 == 10 = (:) (Normal $ Spare r3) <$> step (r3:rs, idx + 1)
--       | r1 + r2 < 10  = (:) (Normal $ Open (r1, r2)) <$> step (r3:rs, idx + 1)
--       | otherwise     = Left IncompleteGame -- TODO:


-- type FrameAcc = Int

-- makeFramePs :: [Roll] -> Either BowlingError [FrameP]
-- makeFramePs rs = undefined
--   where
--     f [] = roll


-- data FoldFrame
--   = Done Frame
--   | Pend Roll


-- roll :: Roll -> Index -> FoldFrame -> Either BowlingError FoldFrame
-- -- roll r 19
-- -- roll r 20
-- -- roll r 21
-- roll r i (Done _)
--   | r == 10  = Done $ Normal $ Strike
-- -- roll r i (Done _)  = if validate r then Right $ Pend r else Left $ InvalidRoll i r
-- roll r i (Pend p) = undefined


-- validate :: Roll -> Bool
-- validate r = 0 <= r && r <= 10





-- scorePerFrame :: FrameP -> Int
-- scorePerFrame (Normal (Open (r1, r2)))   = r1 + r2
-- scorePerFrame (Normal (Spare n))         = 10 + n
-- scorePerFrame (Normal (Strike (n1, n2))) = 10 + n1 + n2
-- scorePerFrame (Last rs)                  = sum rs



-- 必要条件
-- ボーリングのゲームのスコアを記録するコードを書きなさい。つの操作をサポートすること：

-- roll(pins : int) はプレイヤーがボールを転がすたびに呼ばれる。引数は倒したピンの数。
-- score() : int はゲーム終了時にのみ呼ばれる。これはそのゲームの合計得点を返す。
-- この演習を完了するには、ボウリングの一連の転がりを受け取り、最終的なスコアまたは適切なエラーを返す関数scoreを実装する必要があります：

-- これらの定義はすでに用意されていますが、関数を定義するのはあなた次第です。

-- テスト・スイートは、ロールに0から始まる番号が振られることを期待していることに留意してください。



-- リストモナドを使うときれいに書ける説？

-- 適切なデータ構造を見つけるのが難しい
-- 仕様に正確に、型を組んでいくしか無い？


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


