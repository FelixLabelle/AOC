import System.Environment

getFuelReqs :: Integer -> Integer
getFuelReqs weight = weight `div` 3 - 2

-- Let vs where??
-- https://wiki.haskell.org/Let_vs._Where
totalFuelReqs :: Integer -> Integer
totalFuelReqs weight
	| additionalFuel <= 0 = 0
	| otherwise = additionalFuel + totalFuelReqs additionalFuel
    where  additionalFuel = getFuelReqs weight

accurateModuleFuel :: Integer -> Integer
accurateModuleFuel weight = totalFuelReqs weight

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
   -- mapM_ putStrLn linesOfFiles -- map monad, _ does not return an array
   let moduleWeights = map read linesOfFiles
   let feulReqs = sum (map accurateModuleFuel moduleWeights)
   -- let accurateFuelReq = feulReqs + totalFuelReqs feulReqs
   print feulReqs
   