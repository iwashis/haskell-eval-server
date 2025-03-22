#!/bin/bash

# Test the dockerized Haskell evaluation server
# This script sends a test Haskell program to the server and displays the result

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
-- Write to file function
main = do
  writeFile "test.txt" "Hello, World!"
  putStrLn "File written!"
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
if [[ "$RESULT" == *"Error: File operations are not allowed"* ]]; then
  echo -e "${GREEN}Success! The dockerized Haskell evaluation server is working properly and forbids from writeFile.${NC}"
else
  echo -e "${RED}Test failed. The server did not return the expected output.${NC}"
  echo "Check the container logs with: docker logs haskell-eval-server"
fi
