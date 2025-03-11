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

cat > test_fibonacci.hs << 'EOF'
-- Fibonacci function
fibonacci :: Int -> Int
fibonacci n = go 0 1 n
  where
    go a b 0 = a 
    go a b n = go b (a+b) (n-1) 

-- Main function to print Fibonacci sequence
main = do
  putStrLn "Fibonacci Sequence (0-10):"
  putStrLn $ show [fibonacci n | n <- [0..100]]
EOF

echo -e "${GREEN}Test program created:${NC}"
echo "----------------------------------------"
cat test_fibonacci.hs
echo "----------------------------------------"

# Send the test file to the server
echo -e "${YELLOW}Sending program to the evaluation server...${NC}"
echo "----------------------------------------"
RESULT=$(cat test_fibonacci.hs | nc localhost 8080)
echo -e "${GREEN}Result:${NC}"
echo "$RESULT"
echo "----------------------------------------"

# Clean up
rm test_fibonacci.hs

# Check if we got a proper response
if [[ "$RESULT" == *"Fibonacci Sequence"* ]]; then
  echo -e "${GREEN}Success! The dockerized Haskell evaluation server is working properly.${NC}"
else
  echo -e "${RED}Test failed. The server did not return the expected output.${NC}"
  echo "Check the container logs with: docker logs haskell-eval-server"
fi
