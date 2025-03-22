module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (ExitCode (..))
import System.FilePath (combine)
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)

-- Maximum response size (to prevent memory exhaust attacks)
maxResponseSize :: Int
maxResponseSize = 1024 * 1024 -- 1 MB

maxInputSize :: Int
maxInputSize = 1024 * 1024

-- Timeout in seconds:
tout :: Int
tout = 10

-- timeout in microseconds:
evaluationTimeout :: Int
evaluationTimeout = tout * 1000000

main :: IO ()
main = do
    putStrLn "Starting Haskell evaluation server on port 3000..."

    -- Create socket
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 3000 0)
    listen sock 5

    putStrLn "Server initialized. Waiting for connections..."

    -- Accept connections and handle them
    forever $ do
        (conn, addr) <- accept sock
        putStrLn $ "Connection established from: " ++ show addr
        forkIO $ handleConnection conn `catch` handleException conn addr

-- Handle an individual client connection
handleConnection :: Socket -> IO ()
handleConnection conn = do
    msg <- recv conn 4096

    if BS.null msg
        then return ()
        else do
            let haskellCode = BS.unpack msg
            putStrLn "Received code for evaluation:"
            putStrLn "----------------------------------------"
            putStrLn haskellCode
            putStrLn "----------------------------------------"

            -- Evaluate the Haskell code using GHC directly (not hint)
            result <- evaluateWithGHC haskellCode

            -- Send the result back to the client
            let response =
                    if length result > maxResponseSize
                        then take maxResponseSize result ++ "\n... (response truncated)"
                        else result

            sendAll conn $ BS.pack response

            -- Close the connection
            close conn

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

-- Validate ByteString input size directly
validateInputSize :: String -> Either String String
validateInputSize code
    | inputSize > maxInputSize = Left $ "Error: Input size exceeds " ++ show maxInputSize ++ " bytes (current size: " ++ show inputSize ++ " bytes)."
    | otherwise = Right code
  where
    inputSize = BS.length (BS.pack code)

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

-- Modified evaluateWithGHC function with robust cleanup handling
evaluateWithGHC :: String -> IO String
evaluateWithGHC code = do
    putStrLn "Starting evaluation with GHC..."

    -- Validate imports before evaluation
    case validateImports code >>= validateInputSize of
        Left errorMsg -> return errorMsg
        Right validCode -> do
            -- Create a temporary directory outside the timeout scope
            -- so cleanup will happen even if evaluation times out
            withSystemTempDirectory "eval_dir" $ \tempDir -> do
                let filePath = tempDir `combine` "eval.hs"

                -- Create and write to the file
                bracket
                    (openFile filePath WriteMode)
                    hClose
                    ( \handle -> do
                        hPutStrLn handle "module Main where"
                        hPutStrLn handle ""
                        hPutStrLn handle validCode
                        hPutStrLn handle ""
                        hPutStrLn handle "-- End of user code"
                    )

                -- Print the file content for debugging
                fileContent <- readFile filePath
                putStrLn "File content:"
                putStrLn fileContent

                -- Only apply timeout to the actual evaluation, not to the directory creation or cleanup
                evalResult <- timeout evaluationTimeout $ do
                    -- Use readProcessWithExitCode
                    let runghcCmd = "cd " ++ tempDir ++ " && runghc " ++ filePath
                    putStrLn $ "Running command: " ++ runghcCmd
                    -- Use shell command instead of createProcess
                    (exitCode, stdout, stder) <- readProcessWithExitCode "sh" ["-c", runghcCmd] ""

                    -- Return appropriate result based on exit code
                    return $ case exitCode of
                        ExitSuccess -> stdout
                        ExitFailure c -> "Error (code " ++ show c ++ "): " ++ stder

                -- Handle the timeout case outside the timeout block
                case evalResult of
                    Nothing -> return $ "Error: Evaluation timed out after " ++ show tout ++ " seconds"
                    Just result -> return result

-- Handle any exceptions that occur during connection handling
handleException :: Socket -> SockAddr -> SomeException -> IO ()
handleException conn addr e = do
    hPutStrLn stderr $ "Error handling connection from " ++ show addr ++ ": " ++ show e
    sendAll conn $ BS.pack $ "Server error: " ++ show e
    close conn
