import Control.Monad (ap, liftM)

type Person = String
father :: Person -> Maybe_ Person
father "son1" = Just_ "father1"
father "son2" = Just_ "father2"
father "son3" = Just_ "father2"
father "father1" = Just_ "parental_grandfather1"
father "father2" = Just_ "parental_grandfather2"
father "mother1" = Just_ "marental_grandfather1"
father "mother2" = Just_ "marental_grandfather2"
father "parental_grandfather1" = Just_ "great_grandfather1"
father "parental_grandfather2" = Just_ "great_grandfather2"
father "marental_grandfather1" = Just_ "great_grandfather3"
father "marental_grandfather2" = Just_ "great_grandfather4"
father "parental_grandmother1" = Just_ "great_grandfather5"
father "parental_grandmother2" = Just_ "great_grandfather6"
father "marental_grandmother1" = Just_ "great_grandfather7"
father "marental_grandmother2" = Just_ "great_grandfather8"
father _ = Nothing_ "Unknown father"

mother :: Person -> Maybe_ Person
mother "son1" = Just_ "mother1"
mother "son2" = Just_ "mother2"
mother "son3" = Just_ "mother2"
mother "father1" = Just_ "parental_grandmother1"
mother "father2" = Just_ "parental_grandmother2"
mother "mother1" = Just_ "marental_grandmother1"
mother "mother2" = Just_ "marental_grandmother2"
mother _ = Nothing_ "Unknown father"

data Maybe_ a = Just_ a | Nothing_ String deriving (Show)

instance Functor Maybe_ where
    fmap = liftM

instance Applicative Maybe_ where
    pure = return
    (<*>) = ap

instance Monad Maybe_ where
    return x = Just_ x
    (Just_ x) >>= f = f x
    (Nothing_ s) >>= f = Nothing_ $ s

mothersPaternalGrandfather :: Person -> Maybe_ Person
mothersPaternalGrandfather s = (return s) >>= mother >>= father >>= father

main :: IO ()
main = do
    print $ mothersPaternalGrandfather "son1"
    print $ mothersPaternalGrandfather "son2"
    print $ mothersPaternalGrandfather "son3"
    print $ mothersPaternalGrandfather "father2"
    print $ mothersPaternalGrandfather "great_grandfather2"

