import Data.List (sortBy)
import Data.Function (on)

frequencies :: [Char] -> [(Char, Int)]
frequencies [] = []
frequencies strToCount = sortfrequencies (auxFrequencies strToCount [] [])

        
auxFrequencies [] checkedChars lstCharOccurrence = lstCharOccurrence
auxFrequencies strToCount checkedChars lstCharOccurrence =
    if elem (head strToCount) checkedChars
        then auxFrequencies (tail strToCount) checkedChars lstCharOccurrence
        else auxFrequencies (tail strToCount) (checkedChars ++ [(head strToCount)]) (lstCharOccurrence ++ [((head strToCount), (length (filter (==(head strToCount)) strToCount)))])

sortfrequencies :: Ord a => [(a1, a)] -> [(a1, a)]
sortfrequencies [] = []
sortfrequencies tuplesToSort = sortBy (flip compare `on` snd) tuplesToSort