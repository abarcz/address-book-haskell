import System.IO
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.Foldable (toList)
import Data.Sequence (fromList, update, elemIndexL)
--test
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

descriptionGroup group = (groupName group) ++ " - size: " ++ (show $ length $ groupContacts $ group)

data Group = Group {
	groupName :: String,	-- must be unique, != "" (special name for all contacts)
	groupContacts :: [Int]	-- ids
} deriving (Show, Eq)



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

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

printDate :: IO ()
printDate = do
	(year, month, day) <- date
	putStrLn $ "Today is: " ++ (show day) ++ "." ++ (show month) ++ "." ++ (show year)

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
		"mod" -> defaultAction state contactList
		"details" -> showContactIo state contactList args
		"find" -> findIo state contactList args
		"groups" -> showGroupList state
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

-- IO_ACTION: Remove contact from current list and from state, return to previous list
removeContactIo :: State -> [Contact] -> [String] -> IO ()
removeContactIo state contactList args = do
	if length args == 1 then do
		let index = read (args !! 0) :: Int	-- TODO check if args[0] is a int
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
	putStrLn "Address Book v0.1 - Piotr Trzpil, Aleksy Barcz"
	printDate
	addressBook <- loadAddressBook
	showContactList (State addressBook) (contacts addressBook)
	return ()

loadAddressBook :: IO AddressBook
loadAddressBook = do
	-- TODO : should load address book from file
	return (AddressBook "default" [(Contact 0 "John" "Smith" "Akasa" "" "" ""), (Contact 1 "John" "Doe" "" "678809902" "" ""), (Contact 2 "Paul" "Johnson" "" "" "" "pj@gmail.com")] [(Group "Private Contacts" [1])])






-- IO_ACTION: Groups list
showGroupList :: State -> IO ()
showGroupList state  = do
	putStrLn ""
	let groupList = groups $ addressBook $ state
	showGroups groupList
	let actionNames = ["add", "rm", "mod", "details", "quit", "return"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		
		"add" -> addGroupIo state
		"rm" -> removeGroupIo state groupList args
		"mod" -> modifyGroup state args
		"details" -> showGroupIo state args
		"return" -> showContactList state (contacts $ addressBook $ state)
		"quit" -> quitProgram state
		otherwise -> do
			printError "Invalid command!"
			showGroupList state
	return ()




-- IO_ACTION: Add group and return to previous list
addGroupIo :: State -> IO ()
addGroupIo (State (AddressBook bookName contacts groups))=do 
	group <- getGroupInfo groups
	let newState = State (AddressBook bookName contacts (group:groups))
	showGroupList newState	



-- IO_ACTION: Creates a new group
getGroupInfo :: [Group] -> IO Group
getGroupInfo groups = do
	-- TODO sanitize input
	putStrLn $ "Name: "
	gName <- getLine
	
	return Group {
		groupName=gName,
		groupContacts = []
	}

-- IO_ACTION: Shows group list
showGroups :: [Group] -> IO()
showGroups groupList = do
	putStrLn "Groups:"
	showGroups' groupList 0

showGroups' [] _ = do return ()
showGroups' (group:xs) n = do
	putStrLn ((show n) ++ ": " ++ descriptionGroup group)
	showGroups' xs (n + 1)


-- IO_ACTION: Gets group by index
getGroupByIndexIo :: [Group] -> [String] -> IO (Maybe Group)
getGroupByIndexIo groupList args = do
	if length args == 1 then do

		let index = read (args !! 0) :: Int	-- TODO check if args[0] is a int
		if (index >= 0) && (index < (length groupList)) then do
			return (Just (groupList !! index))
		else do
			printError "wrong index number"
			return Nothing
	else do
		printError "wrong number of params"
		return Nothing
	

-- IO_ACTION: Shows details of a group
showGroupIo :: State -> [String] -> IO()
showGroupIo state args = do 
	let groupList = groups $ addressBook $ state
	mGroup <- getGroupByIndexIo groupList args
	case mGroup of
		Just group -> do
			putStrLn ""
			fullInfoGroup group (contacts $ addressBook $ state)
			putStrLn "[press Enter to continue]"
			getLine
			showGroupList state
		_ -> do
			showGroupList state

-- IO_ACTION: shows content of a group
fullInfoGroup :: Group -> [Contact] -> IO()
fullInfoGroup group contactList = do
	putStrLn $ "Group name: " ++ (groupName group)
	showContacts $ filter (\c -> (nr c) `elem` (groupContacts group) ) contactList



-- IO_ACTION: Remove group from current list and from state, return to previous list
removeGroupIo :: State -> [Group] -> [String] -> IO ()
removeGroupIo state groupList args = do
	let groupList = groups $ addressBook $ state
	mGroup <- getGroupByIndexIo groupList args
	case mGroup of
		Just group -> do
			let newGroups = filter (\g -> groupName g /= groupName group) groupList
			let newState = State (AddressBook ( bookName $ addressBook $ state )  ( contacts $ addressBook $ state ) newGroups)
			showGroupList newState
		_ -> do
			showGroupList state



-- IO_ACTION: entry function to modify a group
modifyGroup :: State -> [String] -> IO()
modifyGroup state args = do
	let groupList = groups $ addressBook $ state
	mGroup <- getGroupByIndexIo groupList args
	case mGroup of
		Just group -> do
			showModifyGroupScreen state group
		_ -> do
			showGroupList state



-- IO_ACTION: Shows a screen that allows to rename a groups and manipulate its contacts
showModifyGroupScreen :: State -> Group -> IO ()
showModifyGroupScreen state group  = do
	putStrLn ""

	fullInfoGroup group (contacts $ addressBook $ state)

	let actionNames = ["rename", "addContact", "rmContact", "return"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		
		"rename" -> renameGroupIo state group
		"addContact" -> addGroupContactIo state group
		"rmContact" -> removeGroupContactIo state group
		"return" -> showGroupList state
		otherwise -> do
			printError "Invalid command!"
			showModifyGroupScreen state group
	return ()

-- updates given group with a given newGroup
updateGroup :: State -> Group -> Group -> State
updateGroup (State (AddressBook bookName contacts groups)) group newGroup = 
	let groupSeq = fromList groups
	in let index = fromJust $ elemIndexL group groupSeq
	in let newGroups = toList $ update index newGroup $ groupSeq
	in let newState = State (AddressBook bookName contacts (newGroups))
	in newState


-- IO_ACTION: Renames given group
renameGroupIo :: State -> Group -> IO ()
renameGroupIo state group = do 
	putStrLn $ "Choose new name: "
	gName <- getLine
	let newGroup = Group gName (groupContacts group)
	let newState = updateGroup state group newGroup
	showModifyGroupScreen newState newGroup



-- IO_ACTION: Adds a contact to given group
addGroupContactIo :: State -> Group -> IO()
addGroupContactIo state group = do
	let availableContacts = filter (\c -> not ((nr c) `elem` (groupContacts group)) ) (contacts $ addressBook $ state)
	putStrLn $ "Available contacts: "
	showContacts' availableContacts 0
	putStrLn $ "Select contact to add to group: "
	input <- getLine
	let index = read(input) :: Int--TODO sanitize
	let selectedContact = availableContacts !! index
	let newGroup = Group (groupName group) ((nr selectedContact):(groupContacts group))
	
	let newState = updateGroup state group newGroup
	showModifyGroupScreen newState newGroup

-- IO_ACTION: Removes a contact from given group
removeGroupContactIo :: State -> Group -> IO()
removeGroupContactIo state group = do
	let inGroupContacts = filter (\c -> ((nr c) `elem` (groupContacts group)) ) (contacts $ addressBook $ state)
	putStrLn $ "Contacts in current group: "
	showContacts' inGroupContacts 0
	putStrLn $ "Select contact to remove from group: "
	input <- getLine
	let index = read(input) :: Int--TODO sanitize
	let selectedContact = inGroupContacts !! index
	let newGroup = Group (groupName group) (filter (\contNr -> contNr /= (nr selectedContact) ) (groupContacts group))
	
	let newState = updateGroup state group newGroup
	showModifyGroupScreen newState newGroup
