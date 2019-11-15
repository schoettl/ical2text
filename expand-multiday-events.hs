
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
    let includeSeriesIdInOutput = False -- TODO flag?
    input <- getContents
    mapM_ (printExpandedEvents includeSeriesIdInOutput) $ zip [1..] $ lines input

printExpandedEvents :: Bool -> (Int, String) -> IO ()
printExpandedEvents seriesId (i, line) = do
    let start:end:_:rest = words line -- does not preserve consecutive whitespace!
    let (LocalTime startDay _) = parseTimeField start
    let (LocalTime ed et) = parseTimeField end
    let endDayIncl = if et > midnight
                         then ed
                         else addDays (-1) ed
    mapM_ (printEvent (toMaybe seriesId i) $ unwords rest) [startDay..endDayIncl]

printEvent :: Maybe Int -> String -> Day -> IO ()
printEvent i rest day = putStrLn $ unwords [start, end, hours] ++ " " ++ maybe "" ((++" ").show) i ++ rest
    where
        hours = show 24
        start = formatTimeField $ LocalTime day midnight
        end   = formatTimeField $ LocalTime (addDays 1 day) midnight

parseTimeField = parseTimeOrError False defaultTimeLocale formatString

formatTimeField = formatTime defaultTimeLocale formatString

formatString = iso8601DateFormat (Just "%H:%M")

toMaybe True x = Just x
toMaybe False _ = Nothing
