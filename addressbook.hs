import System.IO
import Data.Maybe

-- DATA STRUCTURES

data Contact = Contact {
	nr :: Int,	-- must be unique
	name :: String, 
	surname :: String,
	company :: String,
	phoneNumber :: String,
	email :: String,
	birthday :: String
} deriving (Show)

description contact = name contact ++ " " ++ surname contact

fullInfo contact = "Name: " ++ name contact
	++ "\nSurname: " ++ surname contact
	++ "\nCompany: " ++ company contact
	++ "\nPhone: " ++ phoneNumber contact
	++ "\nEmail: " ++ email contact
	++ "\nBirthday: " ++ birthday contact


data Group = Group {
	groupName :: String,	-- must be unique, != "" (special name for all contacts)
	groupContacts :: [Int]	-- ids
} deriving (Show)


data AddressBook = AddressBook {
	bookName :: String,
	contacts :: [Contact],
	groups :: [Group]
} deriving (Show)


data State = State {
	addressBook :: AddressBook
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

-- remove all occurences of given value from list
dropElement x [] = []
dropElement x (y:xs) = if x == y then dropElement x xs
	else x:(dropElement x xs)


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


-- ACTION "IO_ACTIONS"

-- IO_ACTION: Contacts list
showContactList :: State -> [Contact] -> IO ()
showContactList state contactList = do
	putStrLn ""
	showContacts contactList
	let actionNames = ["quit", "add", "rm", "mod", "details", "find", "groups", "birthday"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		"quit" -> quitProgram state
		"add" -> addContactIo state contactList
		"rm" -> removeContactIo state contactList args
		"mod" -> modifyContactIo state contactList args
		"details" -> showContactIo state contactList args
		"find" -> findIo state contactList args
		"groups" -> defaultAction state contactList		-- TODO : should switch to group window from which groups can be managed
		"birthday" -> defaultAction state contactList	-- TODO
		otherwise -> do
			printError "Invalid command!"
			showContactList state contactList
	return ()

showContacts :: [Contact] -> IO()
showContacts contactList = do
	putStrLn "Contacts:"
	showContacts' contactList 0

showContacts' [] _ = do return ()
showContacts' (contact:xs) n = do
	putStrLn ((show n) ++ ": " ++ description contact)
	showContacts' xs (n + 1)

-- IO_ACTION: modify existing contact
modifyContactIo :: State -> [Contact] -> [String] -> IO ()
modifyContactIo state contactList args = do
	if length args == 1 then do
		let index = read (args !! 0) :: Int -- TODO check if args[0] is an int
		if (index >= 0) && (index < (length contactList)) then do
			putStrLn ""
			newContact <- modifyContactInfo (contactList !! index)
			let (newContacts, removedId) = removeContact contactList index	-- remove old contact from current list
			let newState = removeContactId state removedId	-- remove old contact from state
			let (AddressBook bookName contacts groups) = addressBook newState -- unpack state
			let newState2 = State (AddressBook bookName (newContact:contacts) groups)
			showContactList newState2 (newContact:newContacts)
		else do
			printError "wrong index number"
			showContactList state contactList
	else do
		printError "wrong number of params"
		showContactList state contactList

modifyContactInfo :: Contact -> IO Contact
modifyContactInfo contact = do
	-- TODO sanitize input
	putStrLn $ "Name (" ++ (name contact) ++ "): "
	cName <- getLineWithDefault (name contact)
	putStrLn $ "Surname (" ++ (surname contact) ++ "): "
	cSurname <- getLineWithDefault (surname contact)
	putStrLn $ "Company (" ++ (company contact) ++ "): "
	cCompany <- getLineWithDefault (company contact)
	putStrLn $ "Phone number (" ++ (phoneNumber contact) ++ "): "
	cPhoneNumber <- getLineWithDefault (phoneNumber contact)
	putStrLn $ "Email (" ++ (email contact) ++ "): "
	cEmail <- getLineWithDefault (email contact)
	putStrLn $ "Birthday (" ++ (birthday contact) ++ "): "
	cBirthday <- getLineWithDefault (birthday contact)
	return Contact {
		nr = nr contact,
		name=cName,
		surname=cSurname,
		company=cCompany,
		phoneNumber=cPhoneNumber,
		email=cEmail,
		birthday=cBirthday
	}

-- IO_ACTION: Remove contact from current list and from state, return to previous list
removeContactIo :: State -> [Contact] -> [String] -> IO ()
removeContactIo state contactList args = do
	if length args == 1 then do
		let index = read (args !! 0) :: Int	-- TODO check if args[0] is an int
		if (index >= 0) && (index < (length contactList)) then do
			let (newContacts, removedId) = removeContact contactList index	-- remove contact from current list
			let newState = removeContactId state removedId	-- remove contact from state
			showContactList newState newContacts
		else do
			printError "wrong index number"
			showContactList state contactList
	else do
		printError "wrong number of params"
		showContactList state contactList

-- removes contact from current list given index
removeContact :: [Contact] -> Int -> ([Contact], Int)
removeContact [] _ = ([], 0)
removeContact (x:xs) 0 = (xs, nr x)
removeContact (x:xs) n = (x:rest, nr) where
	(rest, nr) = removeContact xs (n - 1)

-- removes contact from state given contact id
removeContactId :: State -> Int -> State
removeContactId (State (AddressBook bookName contacts groups)) nr =
	State (AddressBook bookName newContacts newGroups) where
		newContacts = removeContactId' contacts nr
		newGroups = removeFromGroups groups nr
removeContactId' (x:xs) nr2 = if (nr x) == nr2 then xs
	else x:(removeContactId' xs nr2)
removeFromGroups :: [Group] -> Int -> [Group]
removeFromGroups [] _ = []
removeFromGroups ((Group groupName ids):xs) nr = (Group groupName (dropElement nr ids)):(removeFromGroups xs nr)

-- IO_ACTION: show details of a contact
showContactIo :: State -> [Contact] -> [String] -> IO ()
showContactIo state contactList args = do
	if length args == 1 then do
		let index = read (args !! 0) :: Int	-- TODO check if args[0] is a int
		if (index >= 0) && (index < (length contactList)) then do
			putStrLn ""
			putStrLn $ fullInfo (contactList !! index)
			putStrLn "[press Enter to continue]"
			getLine
			showContactList state contactList
		else do
			printError "wrong index number"
			showContactList state contactList
	else do
		printError "wrong number of params"
		showContactList state contactList
	

-- IO_ACTION: Add contact and return to previous list
addContactIo :: State -> [Contact] -> IO ()
addContactIo (State (AddressBook bookName contacts groups)) contactList = do
	contact <- getContactInfo contacts
	let newState = State (AddressBook bookName (contact:contacts) groups)
	showContactList newState (contact:contactList)	-- TODO what if we were viewing a group? contact should be added to this group

getContactInfo :: [Contact] -> IO Contact
getContactInfo contacts = do
	-- TODO sanitize input
	putStrLn $ "Name: "
	cName <- getLine
	putStrLn $ "Surname: "
	cSurname <- getLine
	putStrLn $ "Company: "
	cCompany <- getLine
	putStrLn $ "Phone number: "
	cPhoneNumber <- getLine
	putStrLn $ "Email: "
	cEmail <- getLine
	putStrLn $ "Birthday: "
	cBirthday <- getLine
	return Contact {
		nr = generateId contacts,
		name=cName,
		surname=cSurname,
		company=cCompany,
		phoneNumber=cPhoneNumber,
		email=cEmail,
		birthday=cBirthday
	}

-- generate id nr for a new contact
generateId :: [Contact] -> Int
generateId contacts = firstNotUsedId usedIds 0 where
	usedIds = collectIds contacts
firstNotUsedId usedIds n = if elem n usedIds then firstNotUsedId usedIds (n + 1)
	else n
collectIds :: [Contact] -> [Int]
collectIds [] = []
collectIds (x:xs) = (nr x):(collectIds xs)

-- IO_ACTION: search based on a single criterion
-- find group xxx
-- find name xxx
-- find phone xxx etc
findIo :: State -> [Contact] -> [String] -> IO ()
findIo state contactList args = do
	if length args /= 2 then do
		if length args /= 1 then do
			printError "wrong number of params for find"
			showContactList state contactList
		else
			if args !! 0 == "all" then showContactList state (contacts (addressBook state))
			else do
				printError "wrong number of params for find"
				showContactList state contactList
	else do
		let specifier = args !! 0
		case specifier of
			"group" -> do
				let result = findGroup (groups (addressBook state)) (args !! 1)
				if isNothing result then do
					printError "group not found"
					showContactList state contactList
				else do
					let Just group = result
					showContactList state (getContactsByIds (contacts (addressBook state)) (groupContacts group))
			"name" -> do
				showContactList state (findContactsByName (contacts (addressBook state)) (args !! 1))
			"surname" -> do
				showContactList state (findContactsBySurname (contacts (addressBook state)) (args !! 1))
			"company" -> do
				showContactList state (findContactsByCompany (contacts (addressBook state)) (args !! 1))
			"phone" -> do
				showContactList state (findContactsByPhoneNumber (contacts (addressBook state)) (args !! 1))
			"email" -> do
				showContactList state (findContactsByEmail (contacts (addressBook state)) (args !! 1))
			otherwise -> do
				printError "search criterion not implemented"
				showContactList state contactList

-- return group with given name
findGroup :: [Group] -> String -> Maybe Group
findGroup [] _ = Nothing
findGroup (x:xs) name = if groupName x == name then Just x
	else findGroup xs name

-- get contacts with given ids
getContactsByIds :: [Contact] -> [Int] -> [Contact]
getContactsByIds [] _ = []
getContactsByIds (x:xs) ids = if elem (nr x) ids then x:rest
	else rest where
		rest = getContactsByIds xs ids

-- finds all contacts with given name
findContactsByName :: [Contact] -> String -> [Contact]
findContactsByName [] _ = []
findContactsByName (x:xs) value = if name x == value then x:rest
	else rest where
		rest = findContactsByName xs value

findContactsByEmail :: [Contact] -> String -> [Contact]
findContactsByEmail [] _ = []
findContactsByEmail (x:xs) value = if email x == value then x:rest
	else rest where
		rest = findContactsByEmail xs value

findContactsByPhoneNumber :: [Contact] -> String -> [Contact]
findContactsByPhoneNumber [] _ = []
findContactsByPhoneNumber (x:xs) value = if phoneNumber x == value then x:rest
	else rest where
		rest = findContactsByPhoneNumber xs value

findContactsByCompany :: [Contact] -> String -> [Contact]
findContactsByCompany [] _ = []
findContactsByCompany (x:xs) value = if company x == value then x:rest
	else rest where
		rest = findContactsByCompany xs value

findContactsBySurname :: [Contact] -> String -> [Contact]
findContactsBySurname [] _ = []
findContactsBySurname (x:xs) value = if surname x == value then x:rest
	else rest where
		rest = findContactsBySurname xs value

-- IO_ACTION: Quit program, save address book (TODO)
quitProgram :: State -> IO ()
quitProgram state = do
	return ()


-- IO_ACTION: default
defaultAction :: State -> [Contact] -> IO ()
defaultAction state contactList = do
	printError "Not implemented :)"
	showContactList state contactList


-- MAIN FUNCTION
main :: IO ()
main = do
	putStrLn "Address Book v0.1 - Piotr Trzpil, Aleksy Barcz";
	addressBook <- loadAddressBook
	showContactList (State addressBook) (contacts addressBook)
	return ()

loadAddressBook :: IO AddressBook
loadAddressBook = do
	-- TODO : should load address book from file
	return (AddressBook "default" [(Contact 0 "John" "Smith" "Akasa" "" "" ""), (Contact 0 "John" "Doe" "" "678809902" "" ""), (Contact 1 "Paul" "Johnson" "" "" "" "pj@gmail.com")] [(Group "private" [1])])
