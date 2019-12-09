import Text.ParserCombinators.Parsec
import Data.String.CSV

main::IO()
main = do 
	result <- parseFromFile csvFile "tst3.txt"
	print result