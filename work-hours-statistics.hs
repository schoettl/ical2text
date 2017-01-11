
-- work hours statistics
-- input: output of ical2text
-- usage: ical2text < cal.ics | work-hours-statistics

import Control.Monad
import System.IO
import System.Exit
import Data.Char
import Data.List as L
import Data.Map as M
import Data.Maybe
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
    let _:_:hoursField:keysField:rest = words line
    let keys = L.map toUpper keysField :: String
    let hours = maybeRead hoursField
    when (not $ L.null $ takeWhile (/="@@") rest) $
        printError $ "warning: event text is more than just first letters of names"
    when (L.null keys) $
        printError $ "warning: no first letters of names given for "
    when (L.length keys > 6) $
        printError $ "warning: more than 6 first letters of names"
    when (L.length keys /= (L.length . L.nub) keys) $
        die $ "error: duplicate first letters in " ++ keys
    when (isNothing hours) $
        die $ "error: invalid hours value in 3rd column. number expected but saw: " ++ hoursField
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
        printStatLine (n, h) = putStrLn $ printf "%c: %.2f h (%.2f %%)" n h (h / total)
        total :: Float
        total = M.foldl (+) 0 m

maybeRead = fmap fst . listToMaybe . reads

printError :: String -> IO ()
printError = hPutStrLn stderr
