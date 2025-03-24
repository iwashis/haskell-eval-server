module FileSecurityValidator (validateNoFileOps, validateInputSize, validateImports, sanitizeToAsciiOnly) where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum, isAscii)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

maxInputSize :: Int
maxInputSize = 1024 * 1024

-- Check for file operations in the code
validateNoFileOps :: T.Text -> Either String T.Text
validateNoFileOps code =
    let codeLines = T.lines code
        -- Check for file operations in each line
        problematicLines = filter hasFileOperation codeLines
     in if null problematicLines
            then Right code
            else
                Left $
                    "Error: File operations are not allowed.\n"
                        ++ "Found potential file operations in:\n"
                        ++ unlines (map (\line -> "  - " ++ T.unpack line) problematicLines)

-- Check if a line contains file operations
hasFileOperation :: T.Text -> Bool
hasFileOperation line =
    any (`isTokenIn` line) fileOperationPatterns

-- List of patterns that indicate file operations
fileOperationPatterns :: [T.Text]
fileOperationPatterns =
    map
        T.pack
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
isTokenIn :: T.Text -> T.Text -> Bool
isTokenIn token str =
    let paddedStr = T.cons ' ' (T.snoc str ' ') -- Add spaces to handle word boundaries
        possiblePositions = [1 .. (T.length paddedStr - T.length token)]

        -- Check if token exists at a position and is surrounded by non-alphanumeric chars
        isTokenAt pos =
            T.take (T.length token) (T.drop pos paddedStr) == token
                && not (isAlphaNum (T.index paddedStr (pos - 1)))
                && not (isAlphaNum (T.index paddedStr (pos + T.length token)))
     in any isTokenAt possiblePositions

-- Validate input size directly
validateInputSize :: T.Text -> Either String T.Text
validateInputSize code
    | inputSize > maxInputSize = Left $ "Error: Input size exceeds " ++ show maxInputSize ++ " bytes (current size: " ++ show inputSize ++ " bytes)."
    | otherwise = Right code
  where
    inputSize = BS.length (TE.encodeUtf8 code)

-- Whitelist of allowed modules
allowedModules :: [T.Text]
allowedModules =
    map
        T.pack
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
validateImports :: T.Text -> Either String T.Text
validateImports code =
    let importLines = filter (T.isPrefixOf (T.pack "import ")) (T.lines code)
        extractModuleName line =
            case T.words line of
                (imp : qual : modName : _) | imp == T.pack "import" && qual == T.pack "qualified" -> Just modName
                (imp : modName : _) | imp == T.pack "import" -> Just modName
                _ -> Nothing
        importedModules = mapMaybe extractModuleName importLines
        disallowedModules = filter (`notElem` allowedModules) importedModules
     in if null disallowedModules
            then Right code
            else Left $ "Error: Use of restricted modules: " ++ intercalate ", " (map T.unpack disallowedModules)

-- Function to remove all non-ASCII characters
sanitizeToAsciiOnly :: T.Text -> T.Text
sanitizeToAsciiOnly = T.filter isAscii
