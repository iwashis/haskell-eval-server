#!/bin/bash

# Test the dockerized Haskell evaluation server
# This script sends a test Haskell program to the server and displays the result
# Works with the project structure: haskell-eval-server with app/Main.hs and src/Lib.hs

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Testing the dockerized Haskell evaluation server...${NC}"

# Check if the container is running
if ! docker ps | grep -q haskell-eval-server; then
  echo -e "${RED}Error: haskell-eval-server container is not running!${NC}"
  echo "Start it with: docker-compose up -d"
  exit 1
fi

# Create a test Haskell file
echo -e "${YELLOW}Creating test Haskell program...${NC}"

cat > test_file.hs << 'EOF'
module SomeName where
import qualified Data.Monoid as M 

-- # Foldables
myMap' :: (a -> b) -> [a] -> [b]
-- foldl (*) seed [a1,..an] = (seed * a1) * a2 ... * an 
myMap' f list = foldl (\b a ->  b ++ [f a] ) [] list -- (b -> a -> b) -> b -> [a] -> b

-- Przyklad list = ["tomek", "ala"]
-- f = length
-- foldl (\b a ->  b ++ [f a] ) [] list = ([] * "tomek") * "ala"
-- [] * "tomek" = [] ++ [ f "tomek"] = [] ++ [5] = [5]
-- [5] * "ala"  = [5] ++ [ f "ala" ] = [5] ++ [3] = [5,3] 

myMapr :: (a -> b) -> [a] -> [b]
-- foldr (#) seed [a1,..an] = a1 # (a2 # ... (an # seed)) 
-- ( a -> b -> b ) -> b -> [a] -> b
myMapr f list = foldr (\a b -> (f a) : b ) [] list 
myFilter :: (a -> Bool) -> [a] -> [a]
-- foldl (*) seed [a1,..an] = (seed * a1) * a2 ... * an 
-- (b -> a -> b) -> b -> [a] -> b
myFilter f list = foldl g [] list  
  where 
    g b a  = if f a then b ++ [a] else b 

--TODO: napisac z foldr
--
-- 2. **Niestandardowy fold dla drzew**
--
--    Zdefiniuj typ danych dla drzewa:
--    ```haskell
--    data Tree a = Node a [Tree a]
--    ```

data Tree a = Node a [Tree a]
  deriving Show

exampleTree :: Tree Int 
exampleTree = Node 1 [ Node 2 [], Node 5 [Node 6 []]]

exampleTreee :: Tree String 
exampleTreee = Node "tomek" [ Node "ala" [], Node "maciek" [Node "ola" []]]


instance Functor Tree where 
-- fmap :: (a -> b) -> Tree a -> Tree b
 fmap f (Node x list) = Node (f x) (map (fmap f) list)

instance Foldable Tree where
-- ( a -> b -> b ) -> b -> Tree a -> b
  foldr f seed (Node x list) = f x (foldr (\a b ->  foldr f b a) seed list)
  -- (a -> m) -> Tree a -> m
  foldMap f (Node x list) = f x M.<>  (foldr (<>) mempty $ fmap (foldMap f) list)
  

main :: IO()
main = putStrLn $ show $ fmap (\x -> x + 2) exampleTree
EOF

echo -e "${GREEN}Test program created:${NC}"
echo "----------------------------------------"
cat test_file.hs
echo "----------------------------------------"

# Send the test file to the server
echo -e "${YELLOW}Sending program to the evaluation server...${NC}"
echo "----------------------------------------"
RESULT=$(cat test_file.hs | nc localhost 8080)
echo -e "${GREEN}Result:${NC}"
echo "$RESULT"
echo "----------------------------------------"

# Clean up
rm test_file.hs

# Check if we got a proper response
if [[ "$RESULT" == *"Node"* ]]; then
  echo -e "${GREEN}Success! The dockerized Haskell evaluation server is working properly.${NC}"
else
  echo -e "${RED}Test failed. The server did not return the expected output.${NC}"
  echo "Check the container logs with: docker logs haskell-eval-server"
fi
