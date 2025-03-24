{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.SecuritySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand, readProcess, readProcessWithExitCode)
import Test.Hspec

-- | Test case definition
data TestCase = TestCase
    { testName :: String -- Name of the test
    , testDescription :: String -- Description
    , testCode :: T.Text -- Haskell code to send
    , expectedResult :: T.Text -> Bool -- Function to validate result
    , severity :: String -- High, Medium, Low
    }

{- | Connect to the server
-- New style:
-}
connectToServer :: String -> Int -> IO Socket
connectToServer host port = do
    -- Get address info
    addrInfos <-
        getAddrInfo
            (Just (defaultHints{addrSocketType = Stream}))
            (Just host)
            (Just (show port))

    -- Create socket and connect
    case addrInfos of
        (addrInfo : _) -> do
            sock <- socket (addrFamily addrInfo) (addrSocketType addrInfo) (addrProtocol addrInfo)
            connect sock (addrAddress addrInfo)
            return sock
        [] -> error $ "Could not resolve address for " ++ host

-- | Send code to server and get response
sendCodeAndGetResponse :: String -> Int -> T.Text -> IO T.Text
sendCodeAndGetResponse host port code = do
    -- Connect to server
    sock <- connectToServer host port
    -- Send code
    sendAll sock (TE.encodeUtf8 code)
    -- Receive response with timeout
    response <- recvWithTimeout sock 10 -- 10 seconds timeout
    -- Close connection
    close sock
    -- Return response
    return response

-- | Receive response with timeout
recvWithTimeout :: Socket -> Int -> IO T.Text
recvWithTimeout sock timeoutSeconds = do
    let loop accumBuffer remainingTime = do
            if remainingTime <= 0
                then return (T.append accumBuffer "(TIMEOUT)")
                else do
                    -- Try to receive data
                    result <- try $ recv sock 4096
                    case result of
                        Left (e :: SomeException) ->
                            return (T.append accumBuffer (T.pack $ "\nError: " ++ show e))
                        Right chunk ->
                            if BS.null chunk
                                then return accumBuffer
                                else do
                                    -- Sleep a bit before trying again
                                    threadDelay 100000 -- 100ms
                                    loop (T.append accumBuffer (TE.decodeUtf8 chunk)) (remainingTime - 0.1)

    loop "" (fromIntegral timeoutSeconds)

-- | Security test cases
securityTests :: [TestCase]
securityTests =
    [ TestCase
        { testName = "File Read Attempt"
        , testDescription = "Attempts to read /etc/passwd"
        , testCode = "main = do\n  contents <- readFile \"/etc/passwd\"\n  putStrLn contents"
        , expectedResult = \r -> "File operations are not allowed" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "High"
        }
    , TestCase
        { testName = "File Write Attempt"
        , testDescription = "Attempts to write to the filesystem"
        , testCode = "main = writeFile \"security_breach.txt\" \"This is a security test\""
        , expectedResult = \r -> "File operations are not allowed" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "High"
        }
    , TestCase
        { testName = "System Command Execution"
        , testDescription = "Attempts to execute system commands"
        , testCode = "import System.Process\nmain = do\n  out <- readProcess \"ls\" [\"-la\"] \"\"\n  putStrLn out"
        , expectedResult = \r -> "restricted modules" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Critical"
        }
    , TestCase
        { testName = "Network Connection"
        , testDescription = "Attempts to open network connections"
        , testCode = "import Network.Socket\nmain = do\n  s <- socket AF_INET Stream 0\n  connect s (SockAddrInet 80 0)\n  putStrLn \"Connected\""
        , expectedResult = \r -> "restricted modules" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "High"
        }
    , TestCase
        { testName = "Memory Exhaustion"
        , testDescription = "Attempts to allocate large amounts of memory"
        , testCode = "main = print $ sum [1..10^9]"
        , expectedResult = \r -> "timed out" `T.isInfixOf` r || "heap" `T.isInfixOf` r || "memory" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Medium"
        }
    , TestCase
        { testName = "CPU Exhaustion"
        , testDescription = "Attempts to create an infinite loop"
        , testCode = "main = let loop = loop in loop"
        , expectedResult = \r -> "timed out" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Medium"
        }
    , TestCase
        { testName = "Process Spawning"
        , testDescription = "Attempts to spawn processes"
        , testCode = "import System.Process\nmain = do\n  _ <- createProcess (shell \"echo 'test' > /tmp/test.txt\")\n  putStrLn \"Process created\""
        , expectedResult = \r -> "restricted modules" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "High"
        }
    , TestCase
        { testName = "Environment Variable Access"
        , testDescription = "Attempts to access environment variables"
        , testCode = "import System.Environment\nmain = do\n  path <- getEnv \"PATH\"\n  putStrLn path"
        , expectedResult = \r -> "restricted modules" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Medium"
        }
    , TestCase
        { testName = "Thread Spawning"
        , testDescription = "Attempts to spawn many threads"
        , testCode = "import Control.Concurrent\nmain = sequence_ $ replicate 1000 $ forkIO $ putStrLn \"Thread spawned\""
        , expectedResult = \r -> not ("Thread spawned" `T.isInfixOf` r && T.count "Thread spawned" r > 100)
        , severity = "Medium"
        }
    , TestCase
        { testName = "Resource Exhaustion - File Descriptors"
        , testDescription = "Attempts to open many file descriptors"
        , testCode = "import System.IO\nmain = do\n  handles <- sequence $ replicate 1000 $ openFile \"/dev/null\" ReadMode\n  putStrLn $ \"Opened \" ++ show (length handles) ++ \" handles\""
        , expectedResult = \r -> "File operations are not allowed" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Medium"
        }
    , TestCase
        { testName = "Unsafe Foreign Function Interface"
        , testDescription = "Attempts to use the FFI to call C functions"
        , testCode = "import Foreign.C.Types\nforeign import ccall \"stdlib.h system\" c_system :: CString -> IO CInt\nmain = putStrLn \"FFI imported\""
        , expectedResult = \r -> "Error" `T.isInfixOf` r || "not allowed" `T.isInfixOf` r || "restricted" `T.isInfixOf` r
        , severity = "Critical"
        }
    , TestCase
        { testName = "Template Haskell"
        , testDescription = "Attempts to use Template Haskell"
        , testCode = "{-# LANGUAGE TemplateHaskell #-}\nimport Language.Haskell.TH\nmain = $(runIO (putStrLn \"TH executed\") >> [| putStrLn \"Hello\" |])"
        , expectedResult = \r -> "Error" `T.isInfixOf` r || "not allowed" `T.isInfixOf` r || "restricted" `T.isInfixOf` r
        , severity = "High"
        }
    , TestCase
        { testName = "GHC API"
        , testDescription = "Attempts to use the GHC API to load and execute code"
        , testCode = "import GHC\nimport GHC.Paths\nmain = putStrLn \"GHC API imported\""
        , expectedResult = \r -> "restricted modules" `T.isInfixOf` r || "Error" `T.isInfixOf` r
        , severity = "Critical"
        }
    , TestCase
        { testName = "Large Output"
        , testDescription = "Attempts to generate a very large output"
        , testCode = "main = putStrLn $ replicate (10*1024*1024) 'a'"
        , expectedResult = \r -> "truncated" `T.isInfixOf` r || T.length r < 10 * 1024 * 1024
        , severity = "Medium"
        }
    ]

-- | Run a single test case
runTestCase :: String -> Int -> TestCase -> IO (Bool, String)
runTestCase host port test = do
    putStrLn $ "\n-- Testing: " ++ testName test ++ " (" ++ severity test ++ ") --"
    putStrLn $ "Description: " ++ testDescription test

    -- Send code to server
    result <- try $ sendCodeAndGetResponse host port (testCode test)

    case result of
        Left (e :: SomeException) -> do
            let errorMsg = "Error: " ++ show e
            putStrLn errorMsg
            return (False, errorMsg)
        Right response -> do
            let passed = expectedResult test response
            putStrLn $
                if passed
                    then "✓ Test passed: Server properly blocked or restricted the operation"
                    else "✗ Test failed: Server did not properly handle the security test"

            putStrLn $
                "Response: "
                    ++ T.unpack (T.take 100 response)
                    ++ (if T.length response > 100 then "..." else "")

            return (passed, T.unpack response)

-- | Run all security tests
runAllTests :: String -> Int -> IO Bool
runAllTests host port = do
    putStrLn $ "Running security test suite against " ++ host ++ ":" ++ show port
    putStrLn $ "Total test cases: " ++ show (length securityTests)

    results <- mapM (runTestCase host port) securityTests

    let totalTests = length results
        passedTests = length $ filter fst results
        failedTests = totalTests - passedTests

    putStrLn "\n=== Security Test Results ==="
    putStrLn $ "Total tests: " ++ show totalTests
    putStrLn $
        "Passed: "
            ++ show passedTests
            ++ " ("
            ++ show (100 * passedTests `div` totalTests)
            ++ "%)"
    putStrLn $ "Failed: " ++ show failedTests

    -- Print failed tests if any
    when (failedTests > 0) $ do
        putStrLn "\nFailed tests:"
        let failedTestNames = [testName test | (test, (passed, _)) <- zip securityTests results, not passed]
        forM_ failedTestNames $ \name ->
            putStrLn $ "  - " ++ name

    return (failedTests == 0)

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

-- | Start the server using the run.sh script
startDocker :: IO ()
startDocker = do
    putStrLn "Starting Docker container with Haskell server..."

    -- Run the run.sh script to build and start Docker container
    callCommand "./run.sh"

    -- Give some time for the container to start
    putStrLn "Waiting for Docker container to initialize..."
    threadDelay 5000000 -- Wait 5 seconds

    -- Check if container is running
    (exitCode, stdout, _) <- readProcessWithExitCode "docker" ["ps", "--filter", "publish=8080", "--format", "{{.Names}}"] ""

    if null stdout
        then error "Docker container failed to start or port 8080 is not exposed"
        else putStrLn $ "Docker container started: " ++ stdout

-- | Stop the Docker container
stopDocker :: IO ()
stopDocker = do
    putStrLn "Stopping Docker container..."

    -- Find container ID by port
    (_, containerId, _) <- readProcessWithExitCode "docker" ["ps", "--filter", "publish=8080", "--format", "{{.ID}}"] ""

    if null containerId
        then putStrLn "No container to stop"
        else do
            -- Stop and remove the container
            _ <- readProcess "docker" ["stop", containerId] ""
            -- _ <- readProcess "docker" ["rm", containerId] ""
            putStrLn "Docker container stopped"

host = "localhost"
port = 8080

-- | Main spec for hspec
spec :: Spec
spec = do
    beforeAll_ startDocker $
        afterAll_ stopDocker $
            describe "Security Tests" $ do
                it "should prevent file system access" $ do
                    result <- runTestCase host port (securityTests !! 0)
                    fst result `shouldBe` True

                it "should prevent system command execution" $ do
                    result <- runTestCase host port (securityTests !! 2)
                    fst result `shouldBe` True

                it "should prevent network access" $ do
                    result <- runTestCase host port (securityTests !! 3)
                    fst result `shouldBe` True

                it "should enforce resource limits" $ do
                    result <- runTestCase host port (securityTests !! 4)
                    fst result `shouldBe` True

                it "should prevent FFI usage" $ do
                    result <- runTestCase host port (securityTests !! 10)
                    fst result `shouldBe` True

-- | Main function for standalone execution
main :: IO ()
main = do
    putStrLn "Haskell Evaluation Server Security Test Suite"
    -- Use bracket to ensure Docker container is stopped even if tests fail
    bracket startDocker (const stopDocker) $ \_ -> do
        -- Run security tests
        results <- mapM (runTestCase host port) securityTests

        let totalTests = length results
            passedTests = length $ filter fst results
            failedTests = totalTests - passedTests

        putStrLn "\n=== Security Test Results ==="
        putStrLn $ "Total tests: " ++ show totalTests
        putStrLn $ "Passed: " ++ show passedTests
        putStrLn $ "Failed: " ++ show failedTests

        if failedTests == 0
            then do
                putStrLn "All security tests passed!"
                exitSuccess
            else do
                putStrLn "Some security tests failed!"
                exitFailure
