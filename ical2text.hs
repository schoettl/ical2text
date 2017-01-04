
-- Convert iCalendar format to plain text
-- usage: ical2text < cal.ics

-- :set makeprg=ghc\ %
-- :make

import Prelude hiding (getContents)
import Text.ICalendar
import Text.Printf (printf)
import Data.ByteString.Lazy (getContents)
import Data.Text.Lazy (pack, unpack)
import Data.Default
import Data.Maybe (maybe, fromJust)
import Data.List (intercalate)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale, iso8601DateFormat)
import Data.Time.LocalTime (TimeZone (..), getCurrentTimeZone, utcToLocalTime, localTimeToUTC, hoursToTimeZone)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime, diffUTCTime, addUTCTime)
import Data.String.Utils
import System.Posix.Temp
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    input <- getContents
    (fileUsedInErrorMessages, _) <- mkstemp "/tmp/ical2text-"
    let result = parseICalendar def fileUsedInErrorMessages input
    case result of
        Left msg -> printError msg
        Right (calendars, warnings) -> do
            mapM_ printError warnings
            proceedWithCalendars calendars

proceedWithCalendars :: [VCalendar] -> IO ()
proceedWithCalendars = mapM_ printEvents

printEvents :: VCalendar -> IO ()
printEvents c = do
    tz <- getCurrentTimeZone
    mapM_ (printEvent tz) $ vcEvents c

printEvent :: TimeZone -> VEvent -> IO ()
printEvent tz e = putStrLn $ unwords (take 3 fields) ++ " " ++ concatExtraFields (drop 3 fields)
    where
        fields = map (($e) . ($tz)) [startDate, endDate, duration, summary, description, location]
        concatExtraFields = intercalate " @@ "

startDate :: TimeZone -> VEvent -> String
startDate tz e = maybe "" (formatUTCTime tz . dtStartToUTC tz) $ veDTStart e

endDate :: TimeZone -> VEvent -> String
endDate tz e = case veDTEndDuration e of
                Nothing -> ""
                Just (Left dt) -> formatUTCTime tz $ dtEndToUTC tz dt
                Just (Right dp) -> formatUTCTime tz $ durationToEndUTC tz start dp
                where
                    start = dtStartToUTC tz $ fromJust $ veDTStart e

durationToEndUTC :: TimeZone -> UTCTime -> DurationProp -> UTCTime
durationToEndUTC tz start (DurationProp duration _) = error "cannot handle DurationProp in DTEND"

formatUTCTime :: TimeZone -> UTCTime -> String
formatUTCTime tz dt = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M")) $ utcToLocalTime tz dt

duration :: TimeZone -> VEvent -> String
duration tz e = printf "%.2f" (realToFrac (diffUTCTime end start) / 60 / 60 :: Float)
    where
        start = case veDTStart e of
                    Just dt -> dtStartToUTC tz dt
                    Nothing -> error "DTSTART is missing"
        end = case veDTEndDuration e of
                    Just (Left dt) -> dtEndToUTC tz dt
                    Just (Right dp) -> durationToEndUTC tz start dp
                    Nothing -> oneHourFrom start -- instead of: error "DTEND is missing"

oneHourFrom :: UTCTime -> UTCTime
oneHourFrom = addUTCTime diff
    where
        diff = fromIntegral 3600 -- seconds

description :: TimeZone -> VEvent -> String
description tz = maybe "" (eliminateLineBreaks . unpack . descriptionValue) . veDescription

summary :: TimeZone -> VEvent -> String
summary tz = maybe "" (eliminateLineBreaks . unpack . summaryValue) . veSummary

location :: TimeZone -> VEvent -> String
location tz = maybe "" (eliminateLineBreaks . unpack . locationValue) . veLocation

printError :: String -> IO ()
printError = hPutStrLn stderr

dtStartToUTC :: TimeZone -> DTStart -> UTCTime
dtStartToUTC tz (DTStartDateTime dt _) = dtToUTC tz dt
dtStartToUTC tz (DTStartDate d _) = dToUTC tz d

dtEndToUTC :: TimeZone -> DTEnd -> UTCTime
dtEndToUTC tz (DTEndDateTime dt _) = dtToUTC tz dt
dtEndToUTC tz (DTEndDate d _) = dToUTC tz d

dtToUTC :: TimeZone -> DateTime -> UTCTime
dtToUTC tz (FloatingDateTime localTime) = localTimeToUTC tz localTime
dtToUTC _  (UTCDateTime dt) = dt
dtToUTC _  (ZonedDateTime localTime tzTxt) = localTimeToUTC tz localTime
    where tz = lookupTimeZone $ unpack tzTxt
    -- Data.Time.Zones loadTZFromDB could do the job, but it's in the IO monad :(

lookupTimeZone :: String -> TimeZone
lookupTimeZone "Europe/Berlin" = hoursToTimeZone 1
lookupTimeZone s = error $ "TimeZone not implemented: " ++ s

dToUTC :: TimeZone -> Date -> UTCTime
dToUTC (TimeZone tzMinutes _ _) (Date day) = UTCTime day $ secondsToDiffTime offset
    where offset = fromIntegral $ -1 * tzMinutes * 60

eliminateLineBreaks :: String -> String
eliminateLineBreaks = map replaceCRNL
    where
        replaceCRNL '\n' = ' '
        replaceCRNL '\r' = ' '
        replaceCRNL x = x
-- eliminateLineBreaks = replace "\n\r" "  " -- is not faster but requires MissingH
