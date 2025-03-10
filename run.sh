#!/bin/bash

# Helper script to build and run the Haskell evaluation server in Docker

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo -e "${RED}Error: docker-compose is not installed.${NC}"
    exit 1
fi

# Build and start the container
echo -e "${YELLOW}Building and starting the Haskell evaluation server...${NC}"
docker-compose build && docker-compose up -d

# Check if the container is running
if docker ps | grep -q haskell-eval-server; then
    echo -e "${GREEN}Haskell evaluation server is running!${NC}"
    echo -e "Container ID: $(docker ps -q -f name=haskell-eval-server)"
    
    # Get the port mapping
    PORT=$(docker-compose port haskell-eval-server 3000 | cut -d':' -f2)
    echo -e "Server is accessible at localhost:${PORT}"
    
    echo -e "\n${YELLOW}You can test the server with:${NC}"
    echo -e "echo 'main = putStrLn \"Hello, World!\"' | nc localhost ${PORT}"
    
    echo -e "\n${YELLOW}View logs with:${NC}"
    echo -e "docker-compose logs -f"
    
    echo -e "\n${YELLOW}Stop the server with:${NC}"
    echo -e "docker-compose down"
else
    echo -e "${RED}Failed to start the Haskell evaluation server.${NC}"
    echo -e "Check the logs with: docker-compose logs"
fi
