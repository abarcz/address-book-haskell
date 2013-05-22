{-
	Plan jest taki, aby program składał się z kilku "ekranów", gdzie 
	na każdym jest wypisywana "zawartość ekranu:, następnie dostępne polecenia wraz z opisem, 
	a potem użytkownik wprowadza polecenie. 

	Taka jest moja propozycja (jak masz jakiś lepszy pomysł, to go wprowadź;) :

	Ekran1: główne menu - książka adresowa
	- wyświetlenie skrótów ze wszystkich kontaków (imię i nazwisko) jako ponumerowaną listę
	- wyświetlenie listy nazw grup

		Polecenia:
		- addContact - dodanie nowego kontaktu. Odbywa się poprzez wyświetlenie tekstu "wprowadź ..." 
			i oczekiwania na wprowadzenie danego pola
		- removeContact n - usuniecie kontaktu poprzez podanie indeksu kontaktu na liście
	 	- modifyContact n - może lepiej na kolejnym ekranie?
	 	- detailsContact n - wyświetlenie szczegółów wybranego kontaktu


		- addGroup - dodanie nowej grupy. Odbywa się poprzez wyświetlenie tekstu "wprowadź nazwę" 
			i oczekiwania na wprowadzenie nazwy
		- removeGroup name - usunięcie grupy
		- modifyGroup name - modyfikacja grupy - jej nazwy
		- mergeGroups name1 name2 - scalenie grup. Jako istniejąca pozostaje grupa name1
		- addToGroup n name - dodanie kontaktu n do grupy - może lepiej na ekranie grupy?
		- removeFromGroup n name - usuniecie kontaktu n z grupy - może lepiej na ekranie grupy?


		- searchByAttribute :: AddressBook -> String(AttrName) -> String(AttrVal) -> [Contact]
		- searchByGroup :: AddressBook -> String(GroupName) -> [Contact]
		- searchByBirthday :: AddressBook -> [Contact]

-}

{-

Długo myślałem jak zrobić view/controller i myślę że taki podział na główne jednostki byłby dobry:
1) Bieżąca lista kontaktów (wszystkie/wyniki wyszukiwania/etc) - to co jest zaimplementowane jako showContactList
2) Lista grup (TODO)
Polecenia wydane w obrębie danej jednostki powodują na koniec powrót do niej (tak zrobiłem z showContactList, powrót następuje ew. z inną bieżącą listą kontaktów). Wyjątek : przejście do drugiej jednostki (np. polecenia: 'groups' / 'contacts').

ad1) pozwala na manipulację kontaktami, wyjście z programu, przejście do (2)
ad2) powinna pozwalać na manipulację grupami, wyjście z programu, przejście do (1)
Nie mam póki co pomysłu gdzie wstawić dodawanie/usuwanie kontaktu z grupy (TODO)

Start programu = załadowanie pliku z bazą. Wyjście = zapis. (TODO & TODO).

Pozostałe TODO:
- help <nazwa_komendy>, albo wyświetlamy pełne składnie komend przy każdym wyświetleniu listy co dla find może zająć kawałek miejsca :)
- odporne na błędy funkcje wczytywania danych z wejścia


Moja propozycja komend (niewygodnie jest wpisywać duże litery, bardziej a'la linux ;) ):

Kontakty: (id = numer na aktualnie wyświetlanej liście)
add (przejście do ekranu dodawania)
rm <id>
mod <id>
details <id>
birthday
groups
quit

Wyszukanie wszystkich kontaktów z podaniem warunku:
find name/surname/company/phone/email/group <value>


Grupy:
addgroup (przejście do ekranu dodawania)
rmgroup <name>
modgroup <name>
mergegroups <name1> <name2>
contacts
quit

-}
