module Main where


import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Codec.Picture
import ImgAnalysis

main :: IO ()
main = do
  args <- getArgs
  let fileName =  "new" ++ (head args)
  let inFile = fmap (either (undefined) (\x->x)) $ readImage (head args)
  let outFile = fmap analyze $ inFile
  fmap fst outFile >>= (BS.writeFile fileName)
  --output <- fmap getClustForDebug inFile
  --putStrLn $ ("Image was quantized to " ++ (show (length output)) ++ " colors.")
  --putStrLn $ fmap (\x->x) output
