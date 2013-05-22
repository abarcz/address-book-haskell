import System.IO

-- DATA STRUCTURES

data Contact = Contact {
	name :: String, 
	surname :: String,
	company :: String,
	phoneNumber :: String,
	email :: String,
	birthday :: String
} deriving (Show)

description contact = name contact ++ " " ++ surname contact


data Group = Group {
	groupName :: String,
	groupContacts :: [Contact]
} deriving (Show)


data AddressBook = AddressBook {
	bookName :: String,
	contacts :: [Contact],
	groups :: [Group]
} deriving (Show)


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


-- IO UTILITIES

parseCommandLine :: IO [String]
parseCommandLine = do
	str <- getLine
	tokens <- doSplit str
	return (tokens)


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


-- ACTION SCREENS
-- type for processing address book
type Action = AddressBook -> IO ()


-- SCREEN: Contacts list
showContactList :: Action
showContactList addressBook = do
	showContactEntries (contacts addressBook)
	let actionNames = ["add", "rm", "mod", "show"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		"add" -> defAction addressBook
		"rm" -> defAction addressBook
		--pytanie, czy tworzymy nowe Action dla rm (wyświetla tylko pytanie y/n i usuwa), czy tu zmieniamy (?) zawartość addressBook
		otherwise -> do
			putStrLn "Invalid command!"
			showContactList addressBook
	return ()

showContactEntries :: [Contact] -> IO()
showContactEntries contacts = do
	putStrLn "Contacts:"
	showContactEntry contacts 1

showContactEntry [] _ = do return ()
showContactEntry (contact:xs) n = do
	putStrLn ((show n) ++ ": " ++ description contact)
	showContactEntry xs (n + 1)


-- SCREEN: Add contact
defAction :: Action
defAction addressBook = do
	putStrLn "Not implemented :)"
	return ()


-- MAIN FUNCTION
main :: IO ()
main = do
	addressBook <- loadAddressBook
	showContactList addressBook
	return ()

loadAddressBook :: IO AddressBook
loadAddressBook = do
	-- TODO : should load address book from file
	return (AddressBook "default" [(Contact "John" "Smith" "" "" "" ""), (Contact "Paul" "Johnson" "" "" "" "")] [])
