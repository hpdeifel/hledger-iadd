-- | Some helper functions for "Data.Time"
module Data.Time.Ext
  ( module Data.Time
  , getLocalTime
  , getLocalDay
  ) where

import Data.Time

-- | Return the current time in the system time zone.
getLocalTime :: IO LocalTime
getLocalTime = zonedTimeToLocalTime <$> getZonedTime

-- | Return the current day in the system time zone.
getLocalDay :: IO Day
getLocalDay = localDay <$> getLocalTime
