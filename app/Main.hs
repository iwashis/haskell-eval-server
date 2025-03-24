{-# LANGUAGE  OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import FileSecurityValidator (validateImports, validateInputSize, validateNoFileOps, sanitizeToAsciiOnly)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (ExitCode (..))
import System.FilePath (combine)
import System.IO (stderr)
import Data.Text.IO (hPutStrLn)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Data.Char (isSpace)
import System.Environment (setEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

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
            let decodedText = TE.decodeUtf8 msg
            let haskellCode = sanitizeToAsciiOnly decodedText
            putStrLn "Received code for evaluation:"
            putStrLn "----------------------------------------"
            print haskellCode
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
evaluateWithGHC :: T.Text -> IO String
evaluateWithGHC code = do
    putStrLn "Starting evaluation with GHC..."

    setEnv "LANG" "C.UTF-8"
    setEnv "LC_ALL" "C.UTF-8"

    -- Validate imports before evaluation
    case validateImports cleanedCode >>= validateInputSize >>= validateNoFileOps of
        Left errorMsg -> return errorMsg
        Right validCode -> do
            -- Create a temporary directory outside the timeout scope
            -- so cleanup will happen even if evaluation times out
            withSystemTempDirectory "eval_dir" $ \tempDir -> do
                let filePath = tempDir `combine` "eval.hs"
                let wrapperScript = tempDir `combine` "run_eval.sh"
                -- Use T.concat for complex string building instead of T.unlines with a list
                let fileContent = T.unlines
                      [ "module Main where"
                      , ""
                      , validCode
                      , ""
                      , "-- End of user code"
                      ]
                
                TIO.writeFile filePath fileContent
                
                -- Create the shell script content with proper escaping
                let scriptContent = T.unlines
                      [ "#!/bin/sh"
                      , "# Set TMPDIR to control where GHC creates temporary files"
                      , T.pack $ "export TMPDIR=\"" ++ tempDir ++ "\""
                      , T.pack $ "cd " ++ tempDir
                      , "ulimit -f 0       # No file creation (0 blocks)"
                      , "ulimit -n 32      # Limited file descriptors" 
                      , "ulimit -t 5       # CPU time limit (seconds)"
                      , "# Run with no write access to anything except stdout/stderr"
                      , "export LANG=C.UTF-8"
                      , "export LC_ALL=C.UTF-8"
                      , T.pack $ "cd " ++ tempDir
                      , "exec runghc \\"
                      , "  --ghc-arg=-fpackage-trust \\"
                      , "  --ghc-arg=-dcore-lint \\"
                      , T.pack $ "  " ++ filePath
                      ]
                TIO.writeFile wrapperScript scriptContent

                _ <- system $ "chmod +x " ++ wrapperScript

                TIO.putStrLn fileContent

                -- Only apply timeout to the actual evaluation, not to the directory creation or cleanup
                evalResult <- timeout evaluationTimeout $ do
                    TIO.putStrLn "Running wrapper script: "
                    TIO.putStrLn scriptContent
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
    cleanedCode = ensureMainExists $ filterOutModule code

    filterOutModule c = T.unlines $ filter (not . isModuleDeclaration) $ T.lines c 
    
    -- Function to identify module declaration lines
    isModuleDeclaration :: T.Text -> Bool
    isModuleDeclaration line = 
      let trimmed = T.dropWhile isSpace line
      in T.isPrefixOf (T.pack "module ") trimmed && T.isInfixOf (T.pack " where") trimmed

    hasMainDefinition :: T.Text -> Bool
    hasMainDefinition c = 
      let lines = T.lines c
          -- Look for lines that define main (with common patterns)
          isMainDef line = 
            (T.isPrefixOf (T.pack "main ") (T.stripStart line) && T.isInfixOf (T.pack "=") line) ||
            T.isPrefixOf (T.pack "main::") (T.stripStart line) ||
            T.isPrefixOf (T.pack "main :: ") (T.stripStart line)
      in any isMainDef lines

    -- Function to ensure code has a main definition
    ensureMainExists :: T.Text -> T.Text
    ensureMainExists c =
      if hasMainDefinition c
      then c
      else T.append c (T.pack "\n\nmain :: IO ()\nmain = putStrLn \"No main implemented\"\n")


-- Handle any exceptions that occur during connection handling
handleException :: Socket -> SockAddr -> SomeException -> IO ()
handleException conn addr e = do
    hPutStrLn stderr $ T.append "Error handling connection from " $ T.append (T.pack $ show addr) $ T.append ": " (T.pack $ show e)
    sendAll conn $ BS.pack $ "Server error: " ++ show e
    close conn
