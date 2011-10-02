import qualified Control.Monad as Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import System
import System.IO

type Histogram = Map.Map String Int

-- pad str n will append whitespace to str to create a string of length n
-- (assumes that length str < n)
pad :: String -> Int -> String
pad str n = str ++ replicate (n - length str) ' '

longestKey :: Histogram -> String
longestKey = foldl1 (\acc s -> if length s > length acc then s else acc) . map fst . Map.toList

generateHistogram :: String -> Histogram
generateHistogram str = foldl addWord Map.empty (words $ map toLower str)
    where addWord hist word = Map.insertWith (\_ oldFreq -> oldFreq+1) word 1 hist

printHistogram :: Histogram -> IO ()
printHistogram hist = printPairs $ sortBy (\(k1, a1) (k2, a2) -> compare a2 a1) (Map.toAscList hist)
    where padding :: Int -- in order that all the #s should begin at the same column in the output, we pad words with spaces
          padding = length $ longestKey hist
          maxFreq :: Float -- the highest frequency word gets the greatest number of #s; all other words get a corresponding fraction of this number
          maxFreq = fromIntegral $ foldl1 max $ map snd $ Map.toList hist
          relativeFreq :: Int -> Float
          relativeFreq fr = fromIntegral fr / maxFreq
          numHashes :: Int -> Int -- computes the number of #s to be displayed for a given word, based on that word's relative frequency
          numHashes fr = round $ fromIntegral (76 - padding) * relativeFreq fr
          printPairs :: [(String, Int)] -> IO ()
          printPairs [] = return ()
          printPairs ((k, a):xs) = Monad.unless (numHashes a == 0) $
                                   do putStr (pad k padding)
                                      putStr " : "
                                      putStrLn $ replicate (numHashes a) '#'
                                      printPairs xs

getInput :: [String] -> IO String
getInput []   = getContents -- if no command-line arguments, read from stdin
getInput args = getFileInput [] args
    where getFileInput strs [] = return (concat strs)
          getFileInput strs (filename:xs) = do infile <- openFile filename ReadMode
                                               fileContents <- hGetContents infile
                                               getFileInput ((fileContents ++ " "):strs) xs
          
main = do args <- getArgs
          lines <- getInput args
          printHistogram $ generateHistogram lines
