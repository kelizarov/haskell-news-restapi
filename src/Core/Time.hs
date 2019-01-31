module Core.Time
    ( defaultToday
    , UTCTime
    )
where

import           Data.Time

defaultToday :: UTCTime
defaultToday = UTCTime (fromGregorian 1970 1 1) 0
