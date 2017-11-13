
-- Work hours statistics
-- Each event's title must only consist of a sequnece of letters; one letter per worker.
-- No spaces are allowed. For example, "js" if Jakob and Simon worked at that time.
-- input: output of ical2text
-- usage: ical2text < cal.ics | work-hours-statistics

import Control.Monad
import System.IO
import System.Exit
import qualified Data.Char as C
import Data.List as L
import Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Text.Read
import Text.Printf

type ResultMap = M.Map Char Float

main :: IO ()
main = do
    input <- fmap lines getContents
    result <- foldM processLine empty input
    printResult result

processLine :: ResultMap -> String -> IO ResultMap
processLine map line = do
    let start:_:hoursField:rest = words line
    let keysField = takeWhile (/='@') $ unwords rest
    let keys = T.unpack . T.map C.toUpper . T.strip . T.pack $ keysField :: String
    let hours = maybeRead hoursField
    when (L.null keys) $
        printError $ "warning: no letters given at event " ++ start
    when (L.length keys > 6) $
        printError $ "warning: more than 6 letters of names at event " ++ start
    when (L.length keys /= (L.length . L.nub) keys) $
        die $ "error: duplicate letters in " ++ keys ++ " at event " ++ start
    when (isNothing hours) $
        die $ "error: invalid hours value in 3rd column. number expected but saw " ++ hoursField ++ " at event " ++ start
    foldM (updateResultMap $ fromJust hours) map keys

updateResultMap :: Float -> ResultMap -> Char -> IO ResultMap
updateResultMap hours map key = return $ M.alter (addHours hours) key map

addHours :: Float -> Maybe Float -> Maybe Float
addHours hoursToAdd maybeHours = Just $ maybe hoursToAdd (+hoursToAdd) maybeHours

printResult :: ResultMap -> IO ()
printResult m = do
    putStrLn $ "Arbeitsstunden insgesamt: " ++ show total
    let stat = M.toAscList m
    mapM_ printStatLine stat
    where
        printStatLine :: (Char, Float) -> IO ()
        printStatLine (n, h) = putStrLn $ printf "%c: %.2f h (%.2f %%)" n h (h / total * 100)
        total :: Float
        total = M.foldl (+) 0 m

maybeRead = fmap fst . listToMaybe . reads

printError :: String -> IO ()
printError = hPutStrLn stderr
