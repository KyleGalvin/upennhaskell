import Data.list

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

exactMatches :: Code -> Code -> Int
exactMatches a b = Data.list.foldl + map (== a b)

main = do  
	print (exactMatches [Red, Blue, Green, Yellow] [Blue,Green,Yellow,Red] == 0)