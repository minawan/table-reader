module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text.IO as I hiding (putStrLn)

import TableReader

jsonFile :: FilePath
jsonFile = "ScreenElement.json"

outputLibFile :: FilePath
outputLibFile = "ScreenElement.hs"

outputBinFile :: FilePath
outputBinFile = "GenerateScreenElement.hs"

getRawJSON :: FilePath -> IO B.ByteString
getRawJSON = B.readFile

main :: IO ()
main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String Table)
  case res of
    Left err -> putStrLn err
    Right table -> do
      I.writeFile outputLibFile $ convertToHaskellLib table
      I.writeFile outputBinFile $ convertToHaskellBin table
