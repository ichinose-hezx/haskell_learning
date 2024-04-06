type Borrower = String
type Book = String
type Card = (Borrower, Book)
type Database = [Card]

example :: Database
example = [("Alice Bush", "The Craft of Functional Programming"),("Alice Bush","JB"),("Bob Clinton","Million Dollar Baby")]

books :: Database -> Borrower -> [Book]
books db borrower = [book|(borrower',book)<-db,borrower'==borrower]

borrowers :: Database -> Book -> [Borrower]
borrowers db book = [borrower|(borrower,book')<-db,book'==book]

borrowed :: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((_,book'):rest) book
    | book'==book = True
    | otherwise = borrowed rest book

numBorrowed :: Database -> Borrower -> Int
numBorrowed db borrower = length (books db borrower)

makeLoan :: Database -> Borrower -> Book -> Database
makeLoan db borrower book = [(borrower, book)] ++ db

returnLoan :: Database -> Book -> Database
returnLoan db book = [(borrower,book')|(borrower,book')<-db,book'/=book]

main :: IO()
main = do
    print $ books example "Alice Bush"
    print $ borrowers example "Million Dollar Baby"
    print $ borrowed example "JB"
    print $ numBorrowed example "Alice Bush"
    print $ makeLoan example "Howard" "HFSDVFSd"
    print $ returnLoan example "Million Dollar Baby"