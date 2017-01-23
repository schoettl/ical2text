{-# LANGUAGE QuasiQuotes #-}

-- Convert iCalendar format to plain text
-- usage: ical2text < cal.ics

-- :set makeprg=ghc\ %
-- :make

-- dependencies: icalendar, docopt, regex-compat

import Prelude hiding (getContents)
import Text.Regex
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
import System.Environment (getArgs)
import System.Console.Docopt
import Control.Monad (when)

patterns :: Docopt
patterns = [docopt|ical2text - convert iCalendar to plain text

Usage:
  ical2text [options]

Options:
  -f, --field-separator=STRING
    Field separator used to separate title, description, and location [default: @@].
  -l, --line-separator=STRING
    Line separator used to separate lines in description and location [default: ,,].
  -h, --help
    Print this help message.
|]

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    when (isPresent args $ longOption "help") $
        exitWithUsage patterns
    input <- getContents
    (fileUsedInErrorMessages, _) <- mkstemp "/tmp/ical2text-"
    let result = parseICalendar def fileUsedInErrorMessages input
    case result of
        Left msg -> printError msg
        Right (calendars, warnings) -> do
            mapM_ printError warnings
            proceedWithCalendars args calendars

getArgOrExit = getArgOrExitWith patterns

proceedWithCalendars :: Arguments -> [VCalendar] -> IO ()
proceedWithCalendars args = mapM_ (printEvents args)

printEvents :: Arguments -> VCalendar -> IO ()
printEvents args c = do
    tz <- getCurrentTimeZone
    mapM_ (printEvent args tz) $ vcEvents c

printEvent :: Arguments -> TimeZone -> VEvent -> IO ()
printEvent args tz e = do
    fieldSep <- getArgOrExit args $ longOption "field-separator"
    lineSep  <- getArgOrExit args $ longOption "line-separator"
    let fields = map (($e) . ($tz)) [startDate, endDate, duration, summary lineSep, description lineSep, location lineSep]
    let concatExtraFields = intercalate fieldSep
    putStrLn $ unwords (take nSimpleFields fields) ++ " " ++ concatExtraFields (drop nSimpleFields fields)
    where
        nSimpleFields = 3

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

description :: String -> TimeZone -> VEvent -> String
description lineSep tz = maybe "" (eliminateLineBreaks lineSep . unpack . descriptionValue) . veDescription

summary :: String -> TimeZone -> VEvent -> String
summary lineSep tz = maybe "" (eliminateLineBreaks lineSep . unpack . summaryValue) . veSummary

location :: String -> TimeZone -> VEvent -> String
location lineSep tz = maybe "" (eliminateLineBreaks lineSep . unpack . locationValue) . veLocation

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

eliminateLineBreaks :: String -> String -> String
eliminateLineBreaks lineSep s = subRegex crnlRegex s lineSep
    where
        crnlRegex = mkRegex "\n\r?|\r\n?"
