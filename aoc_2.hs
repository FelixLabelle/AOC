import System.Environment
--import Data.Sequence

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (Prelude.take n l) : (group n (Prelude.drop n l))
  | otherwise = error "Negative or zero n"

updateList :: Int -> a -> [a] -> [a]
updateList pos val x = take pos x ++ [val] ++ drop (pos + 1) x


intCode :: Int -> [Int] -> [Int]
--intCode [] _ = []
intCode counter x
	| cmd == 1 = intCode (counter+1) (updateList pos (v1  + v2) x)
	| cmd == 2 = intCode (counter+1) (updateList pos (v1 * v2) x)
	| cmd == 99 = x
	| otherwise = error "Invalid command"
	where {cmd = x !! (counter*4);
		  pos = (x !! (counter*4 + 3));
		  v1 = x !! (x !! (counter*4 + 1));
		  v2 = x !! (x !! (counter*4 + 2))}

-- figure out how 

testCombos :: Int -> Int -> [Int] -> Int
testCombos 99 99 _ = 99 * 100 + 99
testCombos v1 v2 x 
	| newVal == 19690720 = v1 * 100 + v2
	| otherwise = testCombos (nv1) ((v2+1) `mod` 100) x
	where {
		newList = updateList 2 v2 (updateList 1 v1 x);
		newVal = (intCode 0 newList) !! 0;
		nv1 = if v2 == 99 then v1 + 1 else v1;
	}


main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
   let listOfCommands = map read linesOfFiles
   let groupedCommands = group 4 listOfCommands
   let newCommands = intCode 0 listOfCommands
   --print (newCommands !! 0)
   print(testCombos 0 0 listOfCommands)
   --mapM_ print newCommands
   