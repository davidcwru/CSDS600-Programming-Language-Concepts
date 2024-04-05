-- ghc main.hs -o main.exe

data NestedList a = Element a | SubList [NestedList a] deriving (Show, Eq)

myNestedList :: NestedList Int
myNestedList = SubList [Element 1, Element 3, SubList [Element 4, SubList [SubList [Element 5], SubList []]], Element 6]

flatten :: NestedList a -> [NestedList a]
flatten (Element x) = [Element x]
flatten (SubList xs) = concatMap flatten xs

myreverse :: NestedList a -> NestedList a
myreverse (Element x) = Element x
myreverse (SubList xs) = SubList (reverse (map myreverse xs))

yourfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
yourfunction x maybeList testFunction = do
  list <- maybeList
  if testFunction x then Just (list ++ [x]) else Nothing

checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist xs testFunction = foldr (helper testFunction) (Just []) xs where
  helper test x maybelist = yourfunction x maybelist test

checkappend :: Maybe [a] -> Maybe [a] -> (a -> Bool) -> Maybe [a]
checkappend maybeList1 maybeList2 testFunction = do
    list1 <- maybeList1
    list2 <- maybeList2
    case checklist list1 testFunction of
        Just _  -> Just (list1 ++ list2)
        Nothing -> Nothing

main :: IO ()
main = do
    putStrLn "\nOriginal nested list:"
    print myNestedList

    putStrLn "\nFlattened list:"
    print $ flatten myNestedList

    putStrLn "\nReversed nested list:"
    print $ myreverse myNestedList

    putStrLn "\nchecklist:"
    print $ checklist "aaaaa" (\x -> x == 'a')             -- Output: Just "aaaaa"
    print $ checklist "abcde" (\x -> x >= 'a' && x <= 'z') -- Output: Just "abcde"
    print $ checklist "abcDe" (\x -> x >= 'a' && x <= 'z') -- Output: Nothing
    print $ checklist [1, -2, 3] (\x -> x > 0)             -- Output: Nothing

    putStrLn "\ncheckappend:"
    let result1 = checkappend (Just [1,1,1]) (checkappend (Just [2,3,4]) (Just [8,9]) (\v -> v >= 0)) (\v -> v == 1)
    print result1

    let result2 = checkappend (Just [1,1,1]) (checkappend (Just [2,3,4]) (Just [8,9]) (\v -> v <= 0)) (\v -> v == 1)
    print result2