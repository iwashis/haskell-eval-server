module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import FileSecurityValidator (validateImports, validateInputSize, validateNoFileOps)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (ExitCode (..))
import System.FilePath (combine)
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- Maximum response size (to prevent memory exhaust attacks)
maxResponseSize :: Int
maxResponseSize = 1024 * 1024 -- 1 MB
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

-- Modified evaluateWithGHC function with robust cleanup handling
evaluateWithGHC :: String -> IO String
evaluateWithGHC code = do
    putStrLn "Starting evaluation with GHC..."

    -- Validate imports before evaluation
    case validateImports cleanedCode >>= validateInputSize >>= validateNoFileOps of
        Left errorMsg -> return errorMsg
        Right validCode -> do
            -- Create a temporary directory outside the timeout scope
            -- so cleanup will happen even if evaluation times out
            withSystemTempDirectory "eval_dir" $ \tempDir -> do
                let filePath = tempDir `combine` "eval.hs"
                let wrapperScript = tempDir `combine` "run_eval.sh"

                -- Create and write to the file
                bracket
                    (openFile filePath WriteMode)
                    hClose
                    ( \handle -> do
                        -- hPutStrLn handle "{-# LANGUAGE Safe #-}" 
                        -- hPutStrLn handle ""
                        hPutStrLn handle "module Main where"
                        hPutStrLn handle ""
                        hPutStrLn handle validCode
                        hPutStrLn handle ""
                        hPutStrLn handle "-- End of user code"
                    )
                -- Create a wrapper script with restrictions
                bracket
                    (openFile wrapperScript WriteMode)
                    hClose
                    ( \handle -> do
                        hPutStrLn handle "#!/bin/sh"
                        hPutStrLn handle $ "# Set TMPDIR to control where GHC creates temporary files"
                        hPutStrLn handle $ "export TMPDIR=\"" ++ tempDir ++ "\""
                        hPutStrLn handle $ "cd " ++ tempDir
                        -- Use ulimit to restrict resources
                        hPutStrLn handle "ulimit -f 0       # No file creation (0 blocks)"
                        hPutStrLn handle "ulimit -n 32      # Limited file descriptors"
                        hPutStrLn handle "ulimit -t 5       # CPU time limit (seconds)"
                        hPutStrLn handle "# Run with no write access to anything except stdout/stderr"
                         -- Basic execution with GHC security flags
                        hPutStrLn handle $ "cd " ++ tempDir
                        hPutStrLn handle "exec runghc \\"
                        hPutStrLn handle "  --ghc-arg=-fpackage-trust \\"  -- Trust only core packages
                        -- hPutStrLn handle "  --ghc-arg=-XSafe \\"  -- Enable Safe Haskell
                        hPutStrLn handle "  --ghc-arg=-dcore-lint \\"  -- Extra checking
                        hPutStrLn handle $ "  " ++ filePath
                    )

                -- Make the wrapper script executable
                _ <- system $ "chmod +x " ++ wrapperScript

                -- Print the file content for debugging
                fileContent <- readFile filePath
                putStrLn "File content:"
                putStrLn fileContent

                -- Only apply timeout to the actual evaluation, not to the directory creation or cleanup
                evalResult <- timeout evaluationTimeout $ do
                    scriptContent <- readFile wrapperScript
                    putStrLn "Running wrapper script: "
                    putStrLn scriptContent
                    -- Use shell command instead of createProcess
                    (exitCode, stdout, stder) <- readProcessWithExitCode wrapperScript [] ""

                    -- Return appropriate result based on exit code
                    return $ case exitCode of
                        ExitSuccess -> stdout
                        ExitFailure c -> "Error (code " ++ show c ++ "): " ++ stder

                -- Handle the timeout case outside the timeout block
                case evalResult of
                    Nothing -> return $ "Error: Evaluation timed out after " ++ show tout ++ " seconds"
                    Just result -> return result
  where
    -- Filter out any line that starts with "module" and contains "where"
    cleanedCode = unlines $ filter (not . isModuleDeclaration) $ lines code
    
    -- Function to identify module declaration lines
    isModuleDeclaration :: String -> Bool
    isModuleDeclaration line = 
      let trimmed = dropWhile isSpace line
      in "module " `isPrefixOf` trimmed && " where" `isInfixOf` trimmed

-- Handle any exceptions that occur during connection handling
handleException :: Socket -> SockAddr -> SomeException -> IO ()
handleException conn addr e = do
    hPutStrLn stderr $ "Error handling connection from " ++ show addr ++ ": " ++ show e
    sendAll conn $ BS.pack $ "Server error: " ++ show e
    close conn
