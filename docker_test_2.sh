[1;33mBuilding and starting the Haskell evaluation server...[0m
Compose now can delegate build to bake for better performances
Just set COMPOSE_BAKE=true
#0 building with "desktop-linux" instance using docker driver

#1 [haskell-eval internal] load build definition from Dockerfile
#1 transferring dockerfile: 2.10kB done
#1 WARN: FromAsCasing: 'as' and 'FROM' keywords' casing do not match (line 1)
#1 WARN: FromAsCasing: 'as' and 'FROM' keywords' casing do not match (line 46)
#1 DONE 0.0s

#2 [haskell-eval internal] load metadata for docker.io/library/haskell:9.2.8-slim
#2 DONE 1.1s

#3 [haskell-eval internal] load metadata for docker.io/library/debian:bullseye-slim
#3 DONE 1.1s

#4 [haskell-eval internal] load .dockerignore
#4 transferring context: 2B done
#4 DONE 0.0s

#5 [haskell-eval builder  1/12] FROM docker.io/library/haskell:9.2.8-slim@sha256:a5cf5338ac3b8a1be173a2aab75905e7f5ea6c8efb9d7da2fed8ce0d23c7e44f
#5 DONE 0.0s

#6 [haskell-eval runtime 1/8] FROM docker.io/library/debian:bullseye-slim@sha256:e4b93db6aad977a95aa103917f3de8a2b16ead91cf255c3ccdb300c5d20f3015
#6 DONE 0.0s

#7 [haskell-eval internal] load build context
#7 transferring context: 9.34kB done
#7 DONE 0.0s

#8 [haskell-eval builder  2/12] RUN apt-get update &&     apt-get install -y --no-install-recommends     ca-certificates     g++     gcc     libc6-dev     libffi-dev     libgmp-dev     make     xz-utils     zlib1g-dev     git     && rm -rf /var/lib/apt/lists/*
#8 CACHED

#9 [haskell-eval builder  7/12] COPY README.md ./
#9 CACHED

#10 [haskell-eval builder  4/12] RUN mkdir -p app src
#10 CACHED

#11 [haskell-eval builder  3/12] WORKDIR /opt/haskell-eval-server
#11 CACHED

#12 [haskell-eval builder  6/12] COPY stack.yaml package.yaml ./
#12 CACHED

#13 [haskell-eval builder  5/12] RUN stack config set system-ghc --global true
#13 CACHED

#14 [haskell-eval builder  8/12] RUN stack build --only-dependencies --system-ghc
#14 CACHED

#15 [haskell-eval builder  9/12] COPY app app/
#15 DONE 0.0s

#16 [haskell-eval builder 10/12] COPY src src/
#16 DONE 0.0s

#17 [haskell-eval builder 11/12] RUN stack build --system-ghc --ghc-options="-O2"
#17 3.220 haskell-eval-server> configure (lib + exe)
#17 3.436 Configuring haskell-eval-server-0.1.0.0...
#17 3.519 haskell-eval-server> build (lib + exe) with ghc-9.2.8
#17 3.538 Preprocessing library for haskell-eval-server-0.1.0.0..
#17 3.538 Building library for haskell-eval-server-0.1.0.0..
#17 3.583 [1 of 2] Compiling FileSecurityValidator
#17 4.667 [2 of 2] Compiling Paths_haskell_eval_server
#17 5.119 Preprocessing executable 'haskell-eval-server-exe' for haskell-eval-server-0.1.0.0..
#17 5.119 Building executable 'haskell-eval-server-exe' for haskell-eval-server-0.1.0.0..
#17 5.153 [1 of 2] Compiling Main
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
-- żżżżż
fibonacci :: Integer -> Integer
fibonacci n = go 0 1 n
  where
    go a b 0 = a 
    go a b n = go b (a+b) (n-1) 

-- Main function to print Fibonacci sequence
main = do
  putStrLn "Fibonacci Sequence (0-10):"
  putStrLn $ show [fibonacci n | n <- [0..10]]
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

