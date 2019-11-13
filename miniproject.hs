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

-- function to encode a string
encodingStr :: [Symbol] -> [Char]
encodingStr [] = []
encodingStr str = auxEncodingStr str (startTreeBuild str) []

auxEncodingStr :: [Symbol] -> LTree -> [Char] -> [Char]
auxEncodingStr [] tree encodedStr = encodedStr
auxEncodingStr(x:xs) tree encodedStr = 
    if length xs == 0
        then (encodedStr ++ (encodeChar x tree []))
        else auxEncodingStr xs tree (encodedStr ++ (encodeChar x tree []))

-- function that takes a string and applies all functions that are needed to build a tree
startTreeBuild :: [Char] -> LTree
startTreeBuild str = buildHuffmanTree (sortTreeList (listToLTree (sortfrequencies (frequencies str))))

-- function that creates a list of tuples from a string,
-- where the first value is a character and the second is an integer
frequencies :: Eq a => [a] -> [(a, Int)]
frequencies [] = []
frequencies str = auxFrequencies str [] []

--TODO: add where or let to make it shorter        
auxFrequencies :: Eq a => [a] -> [a] -> [(a, Int)] -> [(a, Int)]
auxFrequencies [] checkedCs lstCOccur = lstCOccur
auxFrequencies str checkedCs lstCOccur =
    if elem (head str) checkedCs
        then auxFrequencies restStr checkedCs lstCOccur
        else auxFrequencies restStr (checkedCs ++ [fstC]) (lstCOccur ++ [(fstC, (occurrenceInStr fstC str))])
        where restStr = tail str
              fstC = head str

occurrenceInStr c str = (length (filter (==c) str))

-- function that sorts a list of tuples based on the value their 2nd value
sortfrequencies :: Ord a => [(a1, a)] -> [(a1, a)]
sortfrequencies [] = []
sortfrequencies tuplesToSort = sortBy (compare `on` snd) tuplesToSort

-- function that sorts a list of LTree based on the frequencies of a Branch/Leaf
sortTreeList :: [LTree] -> [LTree]
sortTreeList treeToSort = sortBy (compare `on` getFrequency) treeToSort

-- check if the list of pairs is a valid association list (the argument won't repeat)
valid :: (Eq a, Eq a1) => [(a1, a)] -> Bool
valid [] = False
valid [a] = True
valid ((a,b):xs)
    | elem a (restArgumentList xs) = False
    | tail xs == [] = True
    | otherwise = valid (head xs : tail xs)

restArgumentList :: [(b, b1)] -> [b]
restArgumentList list = map fst list

--function that merges two LTree types into a new LTree
treemerge :: LTree -> LTree -> LTree
treemerge lt1 lt2 = Branch lt1 lt2 treeSymbols treeFrequency
        where treeSymbols = ((getSymbol lt1) ++ (getSymbol lt2))
              treeFrequency = ((getFrequency lt1) + (getFrequency lt2))

-- function to turn a list of tuples into leaves
listToLTree :: [(Char, Int)] -> [LTree]
listToLTree [] = []
listToLTree lst =
    if valid lst
        then auxListToLTree lst []
        else []

auxListToLTree :: [(Symbol, Frequency)] -> [LTree] -> [LTree]
auxListToLTree [] lstTree = lstTree
auxListToLTree lst lstTree =
    auxListToLTree (tail lst) (lstTree ++ [Leaf (fst (head lst)) (snd (head lst))])

-- function for building a Huffman tree based on a list of leaves
buildHuffmanTree :: [LTree] -> LTree
buildHuffmanTree [t] = t
buildHuffmanTree (t1:t2:ts) = buildHuffmanTree (sortTreeList ((treemerge t1 t2):ts))

-- function to encode one character with the help of a tree
encodeChar :: Symbol -> LTree -> [Char] -> [Char]
encodeChar c (Leaf s f) encoding = encoding
encodeChar c (Branch l r syms f) encoding =
    if elem c (getSymbol l)
        then encodeChar c l (encoding ++ "0")
        else encodeChar c r (encoding ++ "1")

-- function to decode an encoded string with the help of a tree
decodeStr :: [Char] -> LTree -> LTree -> [Symbol] -> [Symbol]
decodeStr [] tree (Leaf s f) decodedStr = (decodedStr ++ [s])
decodeStr enc tree (Leaf s f) decodedStr =  decodeStr enc tree tree (decodedStr ++ [s])
decodeStr enc tree (Branch l r syms f) decodedStr =
    if (head enc) == '0'
        then decodeStr (tail enc) tree l decodedStr
        else decodeStr (tail enc) tree r decodedStr