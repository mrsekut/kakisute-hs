module Clock (addDelta, fromHourMin, toString) where
import           Text.Printf (printf)


-- Clock

newtype Clock = Clock Minute

instance Eq Clock where
  a == b = fromClock a == fromClock b

fromHourMin :: Hour -> Minute -> Clock
fromHourMin hour min = Clock $ toMinute hour + min

toString :: Clock -> String
toString = show . fromClock

addDelta :: Hour -> Minute -> Clock -> Clock
addDelta hour min clock = Clock (c + toMinute hour + min)
  where
    Clock c = clock


-- Internal Clock

newtype InternalClock = InternalClock (Int0to23, Int0to59) deriving (Eq)
type Int0to23 = Int
type Int0to59 = Int

instance Show InternalClock where
  show (InternalClock (h,m)) = printf "%02d:%02d" h m

fromClock :: Clock -> InternalClock
fromClock (Clock c) = InternalClock (h, m)
  where
    h = c `div` 60 `mod` 24
    m = c `mod` 60


-- Utils

type Hour = Int
type Minute = Int

toMinute :: Hour -> Minute
toMinute = (*60)
