module Main where
import Text.Printf
import System.Directory
import Data.List
import Control.Monad
import Control.Arrow
import qualified Data.ByteString.Char8 as BS

(|>) = flip (.)
bi f g h x = f (g x) (h x)

cluster :: (a -> Bool) -> [a] -> [[a]]
cluster p = foldr clus1 [] where
  clus1 x' (c:cs) | p x' = (x':c):cs
  clus1 x' css           = [x']:css

filenames = liftM (map ("ps/" ++)) $ getDirectoryContents "ps"
isMspa = reverse |> take 4 |> reverse |> (== ".txt")
load f = BS.readFile f >>= return . BS.unpack :: IO String
fileContents = filenames >>= sort |> filter isMspa |> mapM load

getDate = lines |> (!! 4) |> read :: String -> Integer
close = uncurry (-) |> abs |> (< 600)
processDates = bi zip id tail |> cluster close |> map (map snd)
processFiles = map getDate |> processDates


main = fileContents >>= processFiles |> print
{-
-}