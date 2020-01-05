{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TableReader
  ( Table
  , convertToHaskellLib
  , convertToHaskellBin
  ) where

import Data.Aeson
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import Text.Printf (printf)

data Table =
  Table { tableName :: !Text
        , tableColumns :: ![Column]
        , tableLevels :: ![Level]
        } deriving (Generic, Show)

data Column =
  Column { columnName :: !Text
         , columnType :: !Text
         } deriving (Generic, Show)

data Level =
  Level { levelName :: !Text
        , sublevels :: ![Text]
        } deriving (Generic, Show)

instance FromJSON Table
instance FromJSON Column
instance FromJSON Level

convertColumnsToHaskell :: [Column] -> Text
convertColumnsToHaskell columns =
    Text.concat . List.intersperse separator $ map declare columns
  where
    separator = "\n  , "
    declare (Column columnName columnType) =
      Text.concat [columnName, " :: !", columnType]

-- Convert varName and a list [(columnName = name_i, columnType = _)] to:
--   varName .: "name_1" [<*> varName .: "name_i"]*
getParseNamedRecordInApplicativeForm :: [Column] -> Text -> Text
getParseNamedRecordInApplicativeForm columns varName =
    Text.concat . List.intersperse separator $ map project names
  where
    separator = " <*> "
    project component = Text.concat [ varName, " .: \"", component, "\""]
    names = map columnName columns

produceTableLiterals :: Text -> [Text] -> String
produceTableLiterals tableName columnNames =
    unlines
      [ firstLetterToLowercase $ printf "%sLiterals :: [String]" tableName
      , firstLetterToLowercase $ printf "%sLiterals =" tableName
      , printf "    [ %s" $ List.intercalate "\n    , "
          . map formatName
          $ tableName : columnNames
      , "    ]"
      ]
  where
    firstLetterToLowercase "" = ""
    firstLetterToLowercase (x:xs) = Char.toLower x : xs
    formatName x = printf "\"%s\"" x

produceColumnLiterals :: Text -> [Text] -> String
produceColumnLiterals tableName columnNames =
    unlines $
      [ printf "produceStringLiteral :: %s -> String" tableName
      , printf "produceStringLiteral x ="
      , printf "  unlines"
      , printf "    [ %s" . List.intercalate "    , \"\"\n    , " $ map callFormatColumn columnNames
      , printf "    ]"
      , printf "  where"
      ] ++ map formatColumn columnNames
  where
    callFormatColumn colName = printf "format%s x" colName
    formatColumn colName = printf "    format%s field = firstLetterToLowercase (%s field) ++ \"Literal :: String\\n\" ++ firstLetterToLowercase (%s field) ++ \"Literal = \\\"\" ++ %s field ++ \"\\\"\"" colName colName colName colName

isStringColumn :: Column -> Bool
isStringColumn (Column _ columnType) = columnType == "String" 

produceFirstLetterToLowercase :: String
produceFirstLetterToLowercase =
    unlines
      [ "firstLetterToLowercase :: String -> String"
      , "firstLetterToLowercase \"\" = \"\""
      , "firstLetterToLowercase (x:xs) = Char.toLower x : xs"
      ]

convertToHaskellLib :: Table -> Text
convertToHaskellLib (Table tableName tableColumns _) =
    Text.pack . unlines $
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "module ScreenElement where"
      , ""
      , "import qualified Data.Char as Char"
      , "import Data.Csv"
      , ""
      , produceFirstLetterToLowercase
      , ""
      , produceTableLiterals tableName columnNames
      , produceColumnLiterals tableName [columnName column | column <- tableColumns, isStringColumn column]
      , printf "data %s = %s" tableName tableName
      , printf "  { %s" (convertColumnsToHaskell tableColumns)
      , "  } deriving (Show)"
      , ""
      , printf "instance FromNamedRecord %s where" tableName
      , printf "  parseNamedRecord record = %s <$> %s" tableName
          (getParseNamedRecordInApplicativeForm tableColumns "record")
      ]
  where columnNames = map columnName tableColumns

convertToHaskellBin :: Table -> Text
convertToHaskellBin (Table tableName _ _) =
    Text.pack . unlines $
      [ "import Data.Csv (Header, decodeByName)"
      , "import qualified Data.ByteString.Lazy as B"
      , "import qualified Data.Vector as Vector"
      , "import Data.Vector (Vector)"
      , "import System.Environment"
      , ""
      , "import ScreenElement"
      , ""
      , "main :: IO ()"
      , "main = do"
      , "  args <- getArgs"
      , printf "  csvData <- B.readFile $ args !! 0"
      , printf "  let decodedCsvData = decodeByName csvData :: Either String (Header, Vector %s)" tableName
      , "  case decodedCsvData of"
      , "    Left err -> putStrLn err"
      , "    Right (_, val) -> do"
      , "      putStrLn $ show val"
      , "      putStrLn . unlines . map produceStringLiteral $ Vector.toList val"
      ]
