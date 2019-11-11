import Data.List (sortBy)
import Data.Function (on)

data LTree = Leaf Char Int | Branch LTree LTree Int deriving Show

frequencies :: [Char] -> [(Char, Int)]
frequencies [] = []
frequencies strToCount = sortfrequencies (auxFrequencies strToCount [] [])

--TODO: add where or let to make it shorter        
auxFrequencies [] checkedChars lstCharOccurrence = lstCharOccurrence
auxFrequencies strToCount checkedChars lstCharOccurrence =
    if elem (head strToCount) checkedChars
        then auxFrequencies restStr checkedChars lstCharOccurrence
        else auxFrequencies restStr (checkedChars ++ [firstC]) (lstCharOccurrence ++ [(firstC, (length (filter (==firstC) strToCount)))])
        where restStr = tail strToCount
              firstC = head strToCount

sortfrequencies :: Ord a => [(a1, a)] -> [(a1, a)]
sortfrequencies [] = []
sortfrequencies tuplesToSort = sortBy (flip compare `on` snd) tuplesToSort





valid :: (Eq a, Eq a1) => [(a1, a)] -> Bool
valid [] = False
valid ((a,b):xs)
    | elem a (restArgumentList xs) = False
    | elem b (restValueList xs) = False
    | tail xs == [] = True
    | otherwise = valid (head xs : tail xs)

restArgumentList :: [(b, b1)] -> [b]
restArgumentList list = map fst list

restValueList :: [(a, b)] -> [b]
restValueList list = map snd list

--although there exist another function called "lookup" which kinda does the same thing
lookup' :: Eq a => [(a, [Char])] -> a -> [Char]
lookup' [] a = "Value does not exist."
lookup' l a = 
    if valid l
        then lookupAux l a
        else "Value does not exsist."

lookupAux :: Eq a => [(a, [Char])] -> a -> [Char]      
lookupAux l a =
    if fst (head l) == a
        then snd (head l)
        else if (length (tail l)) > 0
            then lookupAux (tail l) a
            else "Value does not exist."

findfun :: (t, [Char]) -> [Char] -> [Char]
findfun (a,b) x = 
    if b == x
        then b
        else "Function not found."