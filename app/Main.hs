module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Time (getCurrentTime, diffUTCTime)
import Language.Haskell.Interpreter (InterpreterError(..), runInterpreter, setImports, interpret, as, liftIO)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hPutStrLn, stderr, hClose)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, doesFileExist)
import System.IO.Temp (withSystemTempFile)

-- Maximum response size (to prevent memory exhaust attacks)
maxResponseSize :: Int
maxResponseSize = 1024 * 1024  -- 1 MB

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
      putStrLn $ "Received code for evaluation:"
      putStrLn $ "----------------------------------------"
      putStrLn haskellCode
      putStrLn $ "----------------------------------------"
      
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

-- Evaluate Haskell code using GHC directly instead of hint
evaluateWithGHC :: String -> IO String
evaluateWithGHC code = do
  putStrLn "Starting evaluation with GHC..."
  startTime <- getCurrentTime
  
  -- Create a temporary file with the Haskell code
  result <- withSystemTempFile "eval.hs" $ \filePath handle -> do
    -- Write the code to the file
    hPutStrLn handle "module Main where"
    hPutStrLn handle ""
    hPutStrLn handle code
    hPutStrLn handle ""
    hPutStrLn handle "-- End of user code"
    
    -- Close the file handle
    hClose handle
    
    -- Print the file content for debugging
    fileContent <- readFile filePath
    putStrLn "File content:"
    putStrLn fileContent
    
    -- Compile and run with GHC
    (exitCode, stdout, stderr) <- readProcessWithExitCode "runghc" [filePath] ""
    
    return $ case exitCode of
      ExitSuccess -> stdout
      ExitFailure code -> "Error (code " ++ show code ++ "): " ++ stderr

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "Evaluation completed in " ++ show duration

  return result

-- Handle any exceptions that occur during connection handling
handleException :: Socket -> SockAddr -> SomeException -> IO ()
handleException conn addr e = do
  hPutStrLn stderr $ "Error handling connection from " ++ show addr ++ ": " ++ show e
  sendAll conn $ BS.pack $ "Server error: " ++ show e
  close conn

