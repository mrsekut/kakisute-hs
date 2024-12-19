-- {-# LANGUAGE OverloadedRecordDot #-}

module Record.OverloadedRecordDot where

-- data Quarter = Fall | Winter | Spring
-- data Status = Passed | Failed | Incomplete | Withdrawn

-- data Taken =
--   Taken { year :: Int
--         , term :: Quarter
--         }

-- data Class =
--   Class { result :: Status
--         , taken :: Taken
--         }

-- getResult :: Class -> Status
-- getResult c = c.result -- get

-- setResult :: Class -> Status -> Class
-- setResult c r = c{result = r} -- update

-- setYearTaken :: Class -> Int -> Class
-- setYearTaken c y = c{taken.year = y} -- nested update

-- getResults :: [Class] -> [Status]
-- getResults = map (.result) -- selector

-- getTerms :: [Class]  -> [Quarter]
-- getTerms = map (.taken.term) -- nested selector