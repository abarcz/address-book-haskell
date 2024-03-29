{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls #-}
module SpopBarczTrzpil_AddressBookMain where


import SpopBarczTrzpil_DataStructures
import SpopBarczTrzpil_IOUtils
import System.IO
import Control.Exception
import System.IO.Error(isDoesNotExistError)
import Data.Maybe
import Data.Char
import Data.Foldable (toList)
import Data.Sequence (fromList, update, elemIndexL)
import Text.JSON
import Text.JSON.Generic




-- ACTION "IO_ACTIONS"

-- IO_ACTION: Contacts list
showContactList :: State -> [Contact] -> IO ()
showContactList state contactList = do
	putStrLn ""
	showContacts contactList
	let actionNames = ["quit", "add", "rm", "mod", "details", "find", "groups", "birthday", "help"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		"quit" -> quitProgram state
		"add" -> addContactIo state contactList
		"rm" -> removeContactIo state contactList args
		"mod" -> modifyContactIo state contactList args
		"details" -> showContactIo state contactList args
		"find" -> findIo state contactList args
		"groups" -> showGroupList state
		"birthday" -> do
			showBirthdayIo state contactList
			putStrLn "[press Enter to continue]"
			getLine
			showContactList state contactList
		"help" -> do
			putStrLn "Possible commands are:"
			putStrLn "quit            - exit program"
			putStrLn "add             - add new contact to address book"
			putStrLn "rm <index>      - remove contact with given list index from address book"
			putStrLn "mod <index>     - modify contact with given list index"
			putStrLn "details <index> - show detailed info about contact"
			putStrLn "find name/surname/company/phone/email/group <param> :"
			putStrLn "                lists contacts matching search criterion, <param> can consist of multiple space-separated strings"
			putStrLn "                example: find name John"
			putStrLn "                example: find group Private Contacts"
			putStrLn "find all        - list all contacts"
			putStrLn "groups          - switch to contact groups menu"
			putStrLn "birthday        - show contacts that have birthday today"
			putStrLn "help            - show this menu"
			putStrLn "[press Enter to continue]"
			getLine
			showContactList state contactList
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


-- IO_ACTION: show people with birthday today
showBirthdayIo :: State -> [Contact] -> IO ()
showBirthdayIo state contactList = do
	today <- getCurrentDate
	let birthdays = getBirthdays state today
	if length birthdays == 0 then do
		putStrLn "No birthdays today"
		
	else do
		putStrLn "Today celebrate birthday: "
		showContacts' birthdays 0

getBirthdays :: State -> (Integer, Int, Int) -> [Contact]
getBirthdays (State (AddressBook bookName contacts groups)) date =
	getBirthdays' contacts date

getBirthdays' :: [Contact] -> (Integer, Int, Int) -> [Contact]
getBirthdays' [] _ = []
getBirthdays' (x:xs) date = if (month (birthday x)) == (month date) && (day (birthday x)) == (day date) then x:(getBirthdays' xs date)
	else getBirthdays' xs date
 
-- IO_ACTION: modify existing contact
modifyContactIo :: State -> [Contact] -> [String] -> IO ()
modifyContactIo state contactList args = do
	if length args == 1 then do
		if not (isInt (args !! 0)) then do
			printError "index must be integer"
			showContactList state contactList
		else do
			let index = read (args !! 0) :: Int
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
	putStrLn $ "Name (" ++ (name contact) ++ "): "
	cName <- getValidStringWithDefault isAlphaString (name contact)
	putStrLn $ "Surname (" ++ (surname contact) ++ "): "
	cSurname <- getValidStringWithDefault isAlphaString(surname contact)
	putStrLn $ "Company (" ++ (company contact) ++ "): "
	cCompany <- getLineWithDefault (company contact)
	putStrLn $ "Phone number (" ++ (phoneNumber contact) ++ "): "
	cPhoneNumber <- getValidStringWithDefault isPhoneNumber (phoneNumber contact)
	putStrLn $ "Email (" ++ (email contact) ++ "): "
	cEmail <- getValidStringWithDefault isEmail (email contact)
	putStrLn $ "Birthday (" ++ (formatDate (birthday contact)) ++ "): "
	cBirthday <- readDateWithDefault (birthday contact)
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
		if not (isInt (args !! 0)) then do
			printError "index must be integer"
			showContactList state contactList
		else do
			let index = read (args !! 0) :: Int
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
removeFromGroups ((Group groupName ids):xs) nr = (Group groupName (filter (\elem -> elem /= nr) ids)):(removeFromGroups xs nr)

-- IO_ACTION: show details of a contact
showContactIo :: State -> [Contact] -> [String] -> IO ()
showContactIo state contactList args = do
	if length args == 1 then do
		if not (isInt (args !! 0)) then do
			printError "index must be integer"
			showContactList state contactList
		else do
			let index = read (args !! 0) :: Int
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
	putStrLn $ "Name: "
	cName <- getValidString isAlphaString
	putStrLn $ "Surname: "
	cSurname <- getValidString isAlphaString
	putStrLn $ "Company: "
	cCompany <- getLine
	putStrLn $ "Phone number: "
	cPhoneNumber <- getValidStringWithDefault isPhoneNumber ""
	putStrLn $ "Email: "
	cEmail <- getValidStringWithDefault isEmail ""
	putStrLn $ "Birthday: "
	cBirthday <- readDate
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
	if length args < 2 then do
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
		let param = foldr1 (\x y -> x ++ " " ++ y) (drop 1 args)
		case specifier of
			"group" -> do
				let result = findGroup (groups (addressBook state)) param
				if isNothing result then do
					printError "group not found"
					showContactList state contactList
				else do
					let Just group = result
					showContactList state (getContactsByIds (contacts (addressBook state)) (groupContacts group))
			"name" -> do
				showContactList state (findContactsByName (contacts (addressBook state)) param)
			"surname" -> do
				showContactList state (findContactsBySurname (contacts (addressBook state)) param)
			"company" -> do
				showContactList state (findContactsByCompany (contacts (addressBook state)) param)
			"phone" -> do
				showContactList state (findContactsByPhoneNumber (contacts (addressBook state)) param)
			"email" -> do
				showContactList state (findContactsByEmail (contacts (addressBook state)) param)
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


	let jsonStrState = encodeJSON $ addressBook $ state
	writeFile "addressBookData.json" jsonStrState

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
	showBirthdayIo (State addressBook) (contacts addressBook)
	showContactList (State addressBook) (contacts addressBook)
	return ()

loadAddressBook :: IO AddressBook
loadAddressBook = do
	-- TODO : should load address book from file

	input <- try (readFile "addressBookData.json")
	case input of
		Left e -> do
			if isDoesNotExistError e
	        	then return getDefaultBook
	        	else ioError e
		Right jsonStr -> do 
			let addrBook = decodeJSON jsonStr
			return addrBook

getDefaultBook = (AddressBook "default" [(Contact 0 "John" "Smith" "Akasa" "" "" (1956, 05, 30)), (Contact 1 "John" "Doe" "" "678809902" "" (1988, 06, 01)), (Contact 2 "Paul" "Johnson" "" "" "pj@gmail.com" (1965, 06, 02))] [(Group "Private Contacts" [1])])


-- IO_ACTION: Groups list
showGroupList :: State -> IO ()
showGroupList state  = do
	putStrLn ""
	let groupList = groups $ addressBook $ state
	showGroups groupList
	let actionNames = ["add", "rm", "mod", "details", "help", "return"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		
		"add" -> addGroupIo state
		"rm" -> removeGroupIo state groupList args
		"mod" -> modifyGroup state args
		"details" -> showGroupIo state args
		"return" -> showContactList state (contacts $ addressBook $ state)
		"help" -> do
			putStrLn "Possible commands are:"
			putStrLn "add             - add new group to address book"
			putStrLn "rm <index>      - remove group with given list index from address book"
			putStrLn "mod <index>     - modify group with given list index - switch to modify group menu"
			putStrLn "details <index> - show detailed info about a group with given list index"
			putStrLn "return          - return to contact list menu"
			putStrLn "help            - show this menu"
			putStrLn "[press Enter to continue]"
			getLine
			showGroupList state
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
	
	putStrLn $ "Name: "
	gName <- getValidString isAlphaString
	
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
		if not (isInt (args !! 0)) then do
			printError "index must be integer"
			return Nothing
		else do
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

	let actionNames = ["rename", "addContact", "rmContact", "help" , "return"]
	printCommands actionNames
	(command:args) <- parseCommandLine
	case command of
		
		"rename" -> renameGroupIo state group
		"addContact" -> addGroupContactIo state group
		"rmContact" -> removeGroupContactIo state group
		"return" -> showGroupList state
		"help" -> do
			putStrLn "Possible commands are:"
			putStrLn "rename          - choose a new name for this group"
			putStrLn "addContact      - view available contacts to add to this group and enter list index of a contact to add."
			putStrLn "rmContact       - view contacts to remove from this group and enter list index of a contact to remove."
			putStrLn "return          - return to group list menu"
			putStrLn "help            - show this menu"
			putStrLn "[press Enter to continue]"
			getLine
			showModifyGroupScreen state group
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
	gName <- getValidString isAlphaString

	let newGroup = Group gName (groupContacts group)
	let newState = updateGroup state group newGroup
	showModifyGroupScreen newState newGroup



-- IO_ACTION: Adds a contact to given group
addGroupContactIo :: State -> Group -> IO()
addGroupContactIo state group = do
	let availableContacts = filter (\c -> not ((nr c) `elem` (groupContacts group)) ) (contacts $ addressBook $ state)

	if (length availableContacts) == 0 then do
		printError "No contacts to add."
		showModifyGroupScreen state group
	else do
		putStrLn $ "Available contacts: "
		showContacts' availableContacts 0
		putStrLn $ "Select contact to add to group: "
		input <- getLine
		

		if not (isInt (input )) then do
				printError "index must be integer"
				addGroupContactIo state group
			else do 
				let index = read (input) :: Int	
				if (index >= 0) && (index < (length availableContacts)) then do
					let selectedContact = availableContacts !! index
					let newGroup = Group (groupName group) ((nr selectedContact):(groupContacts group))
					
					let newState = updateGroup state group newGroup
					showModifyGroupScreen newState newGroup
				else do
					printError "wrong index number"
					addGroupContactIo state group


-- IO_ACTION: Removes a contact from given group
removeGroupContactIo :: State -> Group -> IO()
removeGroupContactIo state group = do
	let inGroupContacts = filter (\c -> ((nr c) `elem` (groupContacts group)) ) (contacts $ addressBook $ state)

	if (length inGroupContacts) == 0 then do
		printError "No contacts to remove."
		showModifyGroupScreen state group
	else do
		putStrLn $ "Contacts in current group: "
		showContacts' inGroupContacts 0
		putStrLn $ "Select contact to remove from group: "
		input <- getLine

		if not (isInt (input )) then do
				printError "index must be integer"
				removeGroupContactIo state group
			else do 
				let index = read (input) :: Int	
				if (index >= 0) && (index < (length inGroupContacts)) then do
					let selectedContact = inGroupContacts !! index
					let newGroup = Group (groupName group) (filter (\contNr -> contNr /= (nr selectedContact) ) (groupContacts group))
					let newState = updateGroup state group newGroup
					showModifyGroupScreen newState newGroup

				else do
					printError "wrong index number"
					removeGroupContactIo state group


	