ccNum :: Integer
ccNum = 5594589764218858 --valid cc number
ccNum2 :: Integer
ccNum2 = 1234567898765432 --invalid cc number

--E1

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

--E2

toRevDigits :: Integer -> [Integer]
toRevDigits x
	| x < 1 = []
	| otherwise = lastDigit x : toRevDigits (dropLastDigit x)

--E3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:z)) = x : (y*2) : doubleEveryOther z

--E4

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = (dropLastDigit x) + (lastDigit x)
sumDigits (x:y) = (sumDigits [x]) + (sumDigits y)

--E5

luhn :: Integer -> Bool
luhn x
	| sumDigits (doubleEveryOther (toRevDigits x)) `mod` 10 == 0 = True
	| otherwise = False

--E6

type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi count src buffer target
	| count == 0 = []
	| otherwise = (hanoi (count-1) src target buffer) ++ [(src,target)] ++ (hanoi (count-1) buffer target src)

-- E7 (optional)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 count src bufferSmall bufferLarge target
	| count == 0 = []
	| otherwise = (hanoi4 ((count-2) `div` 2) src target bufferLarge bufferSmall) ++ (hanoi4 ((count-2) `div` 2) src bufferSmall target bufferLarge) ++ (hanoi4 ((count-2) `div` 2) bufferSmall target src bufferLarge) ++ [(src,bufferLarge)] ++ [(src,target)] ++ [(bufferLarge,target)] ++ (hanoi4 ((count-2) `div` 2) bufferLarge bufferLarge target bufferSmall) ++ (hanoi4 ((count-2) `div` 2) bufferLarge target bufferSmall target) ++ (hanoi4 ((count-2) `div` 2) bufferSmall src bufferLarge target)

--End of assignment API. Now we create a function to check our hanoi4 result for correctness, and generalize our hanoi solver to handle arbitrary peg count

type PegState = (Peg,[Integer])
type HanoiState = [PegState]
type HanoiTransitionState = (Integer,HanoiState) --The ring is removed from the pegs and waiting to be placed back on

isLowToHigh :: [Integer] -> Bool
isLowToHigh (x:y:[]) = if (x < y)
	then True
	else False
isLowToHigh [] = True
isLowToHigh x = if (head x < head (tail x))
	then isLowToHigh (tail x)
	else False

hanoiStateValid :: HanoiState -> Bool
hanoiStateValid [] = True
hanoiStateValid s = if ((isLowToHigh (snd(head s))) == True)
	then hanoiStateValid (tail s)
	else False

hanoiMove :: Move -> HanoiState -> HanoiState
hanoiMove (x,y) h = hanoiPushRing y (hanoiPopRing x h)

getTFirstPeg :: HanoiTransitionState -> PegState
getTFirstPeg h = (head (snd h))

getTTailPeg :: HanoiTransitionState -> HanoiState
getTTailPeg ht = if(tail(snd ht) == [])
	then [("null",[])]
	else tail(snd ht)

getFirstPeg :: HanoiState -> PegState
getFirstPeg h = (head h)

getFirstRing :: PegState -> Integer
getFirstRing p = head (snd p)

getPegName :: PegState -> String
getPegName p = fst p

getPegList :: PegState -> [Integer]
getPegList p = snd p

hanoiSingleRingPop :: PegState -> HanoiState
hanoiSingleRingPop p = [((getPegName p),(tail (getPegList p)))]

hanoiPopRing :: Peg -> HanoiState -> HanoiTransitionState
hanoiPopRing p h = hanoiPopRingWorker p [] h

hanoiPopRingWorker :: Peg -> HanoiState -> HanoiState -> HanoiTransitionState
hanoiPopRingWorker pName done (p:x) = if ((getPegName  p) == pName) --if the name matches our first pegstate name
	then (getFirstRing p, (done ++ (hanoiSingleRingPop p) ++ x) ) --take the top ring off the peg, and return the HanoiTransitionState
	else hanoiPopRingWorker pName (done ++ [p]) x -- else shift current peg to our 'done' stack and keep searching

hanoiSingleRingPush :: Integer -> PegState -> HanoiState
hanoiSingleRingPush i p =  [((getPegName p),(i : (getPegList p)))]

hanoiPushRing :: Peg -> HanoiTransitionState -> HanoiState
hanoiPushRing p ht = hanoiPushRingWorker p [] ht

hanoiPushRingWorker :: Peg -> HanoiState -> HanoiTransitionState -> HanoiState
--hanoiPushRingWorker p done (0,[]) = []
hanoiPushRingWorker p done ht = if (fst (getTFirstPeg ht) == p) --check if the current peg name matches our destination peg name
	then (done ++ ( hanoiSingleRingPush (fst ht) (getTFirstPeg ht) )  ++ tail(snd ht)) -- add 'ring' int to current peg, then return 'done' followed by the current peg followed by the remaining pegs
	else hanoiPushRingWorker p (done ++ [getTFirstPeg ht]) ((fst ht),tail(snd ht))

hanoiCheck :: [Move] -> HanoiState -> String
hanoiCheck []  s = "Hanoi Solution Valid"
hanoiCheck ms  s = if (hanoiStateValid s)
	then hanoiCheck (tail ms) (hanoiMove (head ms) s)
	else "Hanoi Solution Invalid"

popTest :: HanoiTransitionState
popTest = ( hanoiPopRing "a" [("a",[1,2,3]),("b",[]),("c",[])] )

main = do  
	print ((luhn ccNum) : (luhn ccNum2) : [])
	print (length(hanoi 15 "a" "b" "c") : length(hanoi4 15 "a" "b" "c" "d") : [])
	print (hanoi4 15 "a" "b" "c" "d")
	print (isLowToHigh [1,2,3,4])
	print (isLowToHigh [1,3,2,4])
	print (head [("a",[1,2,3]),("b",[])])
	print popTest
	print (hanoiPushRing "b" popTest)
	print (hanoiPushRing "c" popTest)
	print (hanoiMove ("a","b") [("a",[1,2,3]),("b",[]),("c",[])] )
	print (hanoiCheck (hanoi4 15 "a" "b" "c" "d") [("a",[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),("b",[]),("c",[]),("d",[])])