import Data.List (sortBy)
import Data.Function (on)

type Symbol = Char
type Frequency = Int

data LTree = Leaf Symbol Frequency
           | Branch LTree LTree [Symbol] Frequency
             deriving Show

getFrequency :: LTree -> Frequency
getFrequency (Leaf _ f) = f
getFrequency (Branch _ _ _ f) = f

getSymbol :: LTree -> [Symbol]
getSymbol (Leaf s _) = [s]
getSymbol (Branch _ _ symbols _) = symbols

frequencies :: [Char] -> LTree
--frequencies [] = []
frequencies strToCount = buildHuffmanTree (listToLTree (reverseFrequencies (sortfrequencies (auxFrequencies strToCount [] []))))

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

sortTreeList :: [LTree] -> [LTree]
sortTreeList treeToSort = sortBy (compare `on` getFrequency) treeToSort

-- check if the list of pairs is a valid association list (the argument won't repeat) CHECK THE TAIL XS PART
valid :: (Eq a, Eq a1) => [(a1, a)] -> Bool
valid [] = False
valid ((a,b):xs)
    | elem a (restArgumentList xs) = False
    | tail xs == [] = True
    | otherwise = valid (head xs : tail xs)

restArgumentList :: [(b, b1)] -> [b]
restArgumentList list = map fst list

-- although there exist another function called "lookup" which kinda does the same thing
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

--function that merges two LTree types into a new LTree
treemerge :: LTree -> LTree -> LTree
treemerge lt1 lt2 = Branch lt1 lt2 treeSymbols treeFrequency
        where treeSymbols = ((getSymbol lt1) ++ (getSymbol lt2))
              treeFrequency = ((getFrequency lt1) + (getFrequency lt2))

-- function that reverses a list
reverseFrequencies :: [(Char, Int)] -> [(Char, Int)]
reverseFrequencies [] = []
reverseFrequencies (x:xs) = reverseFrequencies xs ++ [x]

-- function to turn a list of tuples into leaves
listToLTree :: [(Char, Int)] -> [LTree]
listToLTree [] = []
listToLTree lst =
    if valid lst
        then auxListToLTree lst []
        else []

auxListToLTree [] lstTree = lstTree
auxListToLTree lst lstTree =
    auxListToLTree (tail lst) (lstTree ++ [Leaf (fst (head lst)) (snd (head lst))])

buildHuffmanTree :: [LTree] -> LTree
buildHuffmanTree [t] = t
buildHuffmanTree (t1:t2:ts) = buildHuffmanTree (sortTreeList ((treemerge t1 t2):ts))