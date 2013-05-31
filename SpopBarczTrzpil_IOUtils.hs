{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls #-}
module SpopBarczTrzpil_IOUtils where

import SpopBarczTrzpil_DataStructures

import System.IO
import System.IO.Error
import Data.Maybe
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import Text.JSON
import Text.JSON.Generic

-- IO UTILITIES

parseCommandLine :: IO [String]
parseCommandLine = do
	str <- getLine
	tokens <- doSplit str
	if length tokens > 0 then
		return (tokens)
	else
		parseCommandLine


yesno :: String -> IO Bool
yesno prompt = do
	putStr $ prompt ++ " y/n: "
	hFlush stdout
	str <- getLine
	case str of
		"y" -> return True
		"n" -> return False
		otherwise -> do
			putStrLn "Invalid input."
			yesno prompt


printCommands :: [String] -> IO ()
printCommands commands = do
	putStrLn ("Available commands: " ++ (printableList commands))
	return ()


-- print error and wait for ENTER
printError :: String -> IO ()
printError str = do
	putStrLn $ "ERROR: " ++ str ++ " [press Enter to continue]"
	getLine
	return()

-- gets input, if input == "" returns default
getLineWithDefault :: String -> IO String
getLineWithDefault defaultString = do
	newString <- getLine
	if newString == "" then do
		return defaultString
	else do
		return newString

-- get input, performing a Check (can be noCheck)
getValidString :: Check -> IO String
getValidString check = do
	string <- getLine
	if check string then do
		return string
	else do
		putStrLn "Incorrect format. Try again: "
		getValidString check

-- getLineWithDefault + Check
getValidStringWithDefault :: Check -> String -> IO String
getValidStringWithDefault check defaultString = do
	string <- getLine
	if string == "" then do
		return defaultString
	else do
		if check string then do
			return string
		else do
			putStrLn "Incorrect format. Try again or press Enter to use default: "
			getValidStringWithDefault check defaultString

getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

printDate :: IO ()
printDate = do
	today <- getCurrentDate
	putStrLn $ "Today is: " ++ (formatDate today)

readDate :: IO (Integer, Int, Int)
readDate = do
	date <- getLine
	parseDate date

parseDate :: String -> IO (Integer, Int, Int)
parseDate date = do
	let strings = delimSplit date '.'
	if length strings /= 3 then do
		printError "incorrect format (should be day.month.year)"
		readDate
	else do
		if not (isInt (strings !! 0)) || not (isInt (strings !! 1)) || not (isInt (strings !! 2)) then do
			printError "incorrect format (should be day.month.year)"
			readDate
		else do
			let day = read (strings !! 0) :: Int
			let month = read (strings !! 1) :: Int
			let year = read (strings !! 2) :: Integer
			let validDate = fromGregorianValid year month day
			if isNothing validDate then do
				printError "incorrect date"
				readDate
			else do
				return (year, month, day)

readDateWithDefault :: (Integer, Int, Int) -> IO (Integer, Int, Int)
readDateWithDefault defaultDate = do
	date <- getLine
	if date == "" then do
		return defaultDate
	else do
		parseDate date
