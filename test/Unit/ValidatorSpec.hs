{-# LANGUAGE OverloadedStrings #-}

module Unit.ValidatorSpec (spec) where

import Data.Char (isAscii)
import Data.List (isInfixOf)
import qualified Data.Text as T
import FileSecurityValidator
import Test.Hspec
import Test.QuickCheck

maxInputSize :: Int
maxInputSize = 1024 * 1024

spec :: Spec
spec = do
    describe "FileSecurityValidator" $ do
        describe "sanitizeToAsciiOnly" $ do
            it "keeps ASCII characters" $ do
                sanitizeToAsciiOnly "Hello World!" `shouldBe` "Hello World!"

            it "removes non-ASCII characters" $ do
                sanitizeToAsciiOnly "Hello ∀ World! 😊" `shouldBe` "Hello  World! "

            it "works with empty string" $ do
                sanitizeToAsciiOnly "" `shouldBe` ""

            -- Property-based tests
            it "produces only ASCII characters" $ property $ \s ->
                let sanitized = sanitizeToAsciiOnly (T.pack s)
                 in all isAscii (T.unpack sanitized)

        describe "validateInputSize" $ do
            it "accepts input under size limit" $ do
                validateInputSize "main = putStrLn \"Hello\"" `shouldBe` Right "main = putStrLn \"Hello\""

            it "rejects oversized input" $ do
                let largeInput = T.replicate (2 * maxInputSize) "a"
                case validateInputSize largeInput of
                    Left err -> "Input size exceeds" `shouldSatisfy` (`isInfixOf` err)
                    Right _ -> expectationFailure "Should have rejected oversized input"

            it "works with empty string" $ do
                validateInputSize "" `shouldBe` Right ""

        describe "validateImports" $ do
            it "accepts allowed modules" $ do
                validateImports "import Data.Text\nimport Data.List"
                    `shouldBe` Right "import Data.Text\nimport Data.List"

            it "rejects disallowed modules" $ do
                case validateImports "import System.IO" of
                    Left err -> "restricted modules" `shouldSatisfy` (`isInfixOf` err)
                    Right _ -> expectationFailure "Should have rejected System.IO"

            it "accepts qualified imports of allowed modules" $ do
                validateImports "import qualified Data.Text as T"
                    `shouldBe` Right "import qualified Data.Text as T"

            it "rejects qualified imports of disallowed modules" $ do
                case validateImports "import qualified System.IO as IO" of
                    Left err -> "restricted modules" `shouldSatisfy` (`isInfixOf` err)
                    Right _ -> expectationFailure "Should have rejected qualified System.IO"

            it "properly handles module names with dots" $ do
                validateImports "import Data.List" `shouldSatisfy` isRight

            -- Handle comments and multiline imports
            it "ignores comments when checking imports" $ do
                validateImports "-- import System.IO\nimport Data.Text"
                    `shouldBe` Right "-- import System.IO\nimport Data.Text"

            it "handles multiline import statements" $ do
                validateImports "import Data.List\n  ( sort\n  , nub\n  )"
                    `shouldBe` Right "import Data.List\n  ( sort\n  , nub\n  )"

        describe "validateNoFileOps" $ do
            it "accepts code without file operations" $ do
                validateNoFileOps "main = print (sum [1..10])"
                    `shouldBe` Right "main = print (sum [1..10])"

--
-- it "rejects code with file operations" $ do
--   case validateNoFileOps "main = readFile \"file.txt\"" of
--     Left err -> "File operations are not allowed" `shouldSatisfy` (`isInfixOf` err)
--     Right _ -> expectationFailure "Should have rejected file operations"
--
-- it "detects file operations in function applications" $ do
--   case validateNoFileOps "main = do\n  contents <- readFile \"test.txt\"\n  putStrLn contents" of
--     Left err -> "File operations are not allowed" `shouldSatisfy` (`isInfixOf` err)
--     Right _ -> expectationFailure "Should have detected readFile"

-- it "detects file operations inside let/where bindings" $ do
--   case validateNoFileOps "main = let f = readFile \"test.txt\" in f >>= putStrLn" of
--     Left err -> "File operations are not allowed" `shouldSatisfy` (`isInfixOf` err)
--     Right _ -> expectationFailure "Should have detected readFile in let binding"
--
-- it "ignores file operations in comments" $ do
--   validateNoFileOps "-- readFile \"test.txt\"\nmain = print \"Hello\"" `shouldBe`
--     Right "-- readFile \"test.txt\"\nmain = print \"Hello\""

-- it "ignores file operations in string literals" $ do
--   validateNoFileOps "main = print \"This is not a real readFile call\"" `shouldBe`
--     Right "main = print \"This is not a real readFile call\""

-- Helper function for tests
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
