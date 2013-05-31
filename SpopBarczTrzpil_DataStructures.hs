{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls #-}
module SpopBarczTrzpil_DataStructures where


import Data.Maybe
import Data.Char

import Text.JSON
import Text.JSON.Generic

-- DATA STRUCTURES

data Contact = Contact {
	nr :: Int,	-- must be unique
	name :: String, 
	surname :: String,
	company :: String,
	phoneNumber :: String,
	email :: String,
	birthday :: (Integer, Int, Int)	-- year, month, day
} deriving (Typeable, Data, Show)

description contact = name contact ++ " " ++ surname contact

formatDate (year, month, day) =
	show day ++ "." ++ show month ++ "." ++ show year

year (a, b, c) = a
month (a, b, c) = b
day (a, b, c) = c

fullInfo contact = "Name: " ++ name contact
	++ "\nSurname: " ++ surname contact
	++ "\nCompany: " ++ company contact
	++ "\nPhone: " ++ phoneNumber contact
	++ "\nEmail: " ++ email contact
	++ "\nBirthday: " ++ (formatDate (birthday contact))

descriptionGroup group = (groupName group) ++ " - size: " ++ (show $ length $ groupContacts $ group)

data Group = Group {
	groupName :: String,	-- must be unique, != "" (special name for all contacts)
	groupContacts :: [Int]	-- ids
} deriving (Typeable, Data, Show, Eq)



data AddressBook = AddressBook {
	bookName :: String,
	contacts :: [Contact],
	groups :: [Group]
} deriving (Typeable, Data, Show)


data State = State {
	addressBook :: AddressBook
} deriving (Typeable, Data, Show)


-- UTILITIES

-- return first word from a string and the rest of the string
word :: String -> Char -> (String, String)
word [] _ = ([], [])
word (x:xs) delimiter = if x == delimiter then ([], xs)
	else (x:xrest, rest) where
		(xrest, rest) = word xs delimiter

-- split string using delimiter
delimSplit :: String -> Char -> [String]
delimSplit [] _ = []
delimSplit x delimiter = if rest == [] then [first]
	else (first:(delimSplit rest delimiter)) where
		(first, rest) = word x delimiter

-- split using space as delimiter
doSplit :: String -> IO [String]
doSplit x = do
	return (delimSplit x ' ')


-- concatenate list of strings using multichar delimiter
printableList :: [String] -> String
printableList strings = foldr (++) "" ((map (++ ", ") (take ((length strings) - 1) strings)) ++ ([last strings]))


-- get first index of an element, returns -1 on fail
getIndex x xs = getIndex' x xs 0
getIndex' _ [] _ = -1
getIndex' x (y:xs) n = if x == y then n
	else getIndex' x xs (n + 1)

-- remove all occurences of given value from list
dropElement x [] = []
dropElement x (y:xs) = if x == y then dropElement x xs
	else x:(dropElement x xs)

type Check = String -> Bool
-- all kind of check for strings
noCheck str = True
isInt str = all isDigit str
isAlphaString str = all isAlpha str
isPhoneNumber str = (isInt str) && (length str > 6)
isEmail str = (elem '@' str) && (elem '.' str)
