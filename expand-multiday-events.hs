
-- Expand multi-day events to multiple single-day events
-- usage: ical2text < cal.ics | expand-multiday-events

-- Single-day events are passed through.
-- Multi-day events are splitted to a sequence of full-day events, one per day.

import System.IO
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Format

main :: IO ()
main = do
    input <- getContents
    mapM_ printExpandedEvents $ lines input

printExpandedEvents :: String -> IO ()
printExpandedEvents line = do
    let start:end:_:rest = words line -- does not preserve consecutive whitespace!
    let (LocalTime startDay _) = parseTimeField start
    let (LocalTime ed et) = parseTimeField end
    let endDayIncl = if et > midnight
                         then ed
                         else addDays (-1) ed
    if startDay == endDayIncl
        then putStrLn line
        else mapM_ (printEvents $ unwords rest) [startDay..endDayIncl]

printEvents :: String -> Day -> IO ()
printEvents rest day = putStrLn $ unwords [start, end, hours] ++ " " ++ rest
    where
        hours = show 24
        start = formatTimeField $ LocalTime day midnight
        end   = formatTimeField $ LocalTime (addDays 1 day) midnight

parseTimeField = parseTimeOrError False defaultTimeLocale formatString

formatTimeField = formatTime defaultTimeLocale formatString

formatString = iso8601DateFormat (Just "%H:%M")
