module FileSecurityValidator (validateNoFileOps, validateInputSize, validateImports) where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)

maxInputSize :: Int
maxInputSize = 1024 * 1024

-- Check for file operations in the code
validateNoFileOps :: String -> Either String String
validateNoFileOps code =
    let codeLines = lines code
        -- Check for file operations in each line
        problematicLines = filter hasFileOperation codeLines
     in if null problematicLines
            then Right code
            else
                Left $
                    "Error: File operations are not allowed.\n"
                        ++ "Found potential file operations in:\n"
                        ++ unlines (map ("  - " ++) problematicLines)

-- Check if a line contains file operations
hasFileOperation :: String -> Bool
hasFileOperation line =
    any (`isTokenIn` line) fileOperationPatterns

-- List of patterns that indicate file operations
fileOperationPatterns :: [String]
fileOperationPatterns =
    [ -- File IO functions
      "readFile"
    , "writeFile"
    , "appendFile"
    , "withFile"
    , "openFile"
    , "openBinaryFile"
    , "hGetContents"
    , "hPutStr"
    , "hPutStrLn"
    , "hPrint"
    , "hClose"
    , "IOMode"
    , "ReadMode"
    , "WriteMode"
    , "AppendMode"
    , "ReadWriteMode"
    , "openTempFile"
    , -- Directory operations
      "createDirectory"
    , "removeFile"
    , "removeDirectory"
    , "renameFile"
    , "renameDirectory"
    , "getDirectoryContents"
    , "doesFileExist"
    , "doesDirectoryExist"
    , "getCurrentDirectory"
    , "setCurrentDirectory"
    , "withCurrentDirectory"
    , -- Import statements for file/IO modules
      "import System.IO"
    , "import qualified System.IO"
    , "import System.Directory"
    , "import qualified System.Directory"
    , "import Data.IORef"
    , "import qualified Data.IORef"
    ]

-- Check if a token appears in a string (with word boundaries)
isTokenIn :: String -> String -> Bool
isTokenIn token str =
    let paddedStr = ' ' : str ++ " " -- Add spaces to handle word boundaries
        possiblePositions = [1 .. (length paddedStr - length token)]

        -- Check if token exists at a position and is surrounded by non-alphanumeric chars
        isTokenAt pos =
            take (length token) (drop pos paddedStr) == token
                && not (isAlphaNum (paddedStr !! (pos - 1)))
                && not (isAlphaNum (paddedStr !! (pos + length token)))
     in any isTokenAt possiblePositions

-- Validate ByteString input size directly
validateInputSize :: String -> Either String String
validateInputSize code
    | inputSize > maxInputSize = Left $ "Error: Input size exceeds " ++ show maxInputSize ++ " bytes (current size: " ++ show inputSize ++ " bytes)."
    | otherwise = Right code
  where
    inputSize = BS.length (BS.pack code)

-- Whitelist of allowed modules
allowedModules :: [String]
allowedModules =
    [ -- Basic Prelude modules
      "Prelude"
    , "Data.List"
    , "Data.Maybe"
    , "Data.Char"
    , "Data.Either"
    , "Data.Tuple"
    , "Data.Function"
    , "Data.Ord"
    , "Control.Applicative"
    , "Control.Monad"
    , "Text.Show"
    , "Data.String"
    , "Data.Monoid"
    , -- Useful data structures
      "Data.Map"
    , "Data.Set"
    , "Data.Sequence"
    , "Data.Array"
    , "Data.IntMap"
    , "Data.IntSet"
    , "Data.Tree"
    , -- Text processing
      "Data.Text"
    , "Data.ByteString"
    , -- Safe math operations
      "Numeric"
    , "Data.Complex"
    , "Data.Fixed"
    , "Data.Ratio"
    ]

-- Function to scan code for import statements and validate them
validateImports :: String -> Either String String
validateImports code =
    let importLines = filter (isPrefixOf "import ") (lines code)
        extractModuleName line =
            case words line of
                ("import" : "qualified" : modName : _) -> Just modName
                ("import" : modName : _) -> Just modName
                _ -> Nothing
        importedModules = mapMaybe extractModuleName importLines
        disallowedModules = filter (`notElem` allowedModules) importedModules
     in if null disallowedModules
            then Right code
            else Left $ "Error: Use of restricted modules: " ++ intercalate ", " disallowedModules
