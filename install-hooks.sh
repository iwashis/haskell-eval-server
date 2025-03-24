#!/bin/bash

# Script to install git hooks

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Installing git hooks...${NC}"

# Check if we're in the root of a git repository
if [ ! -d .git ]; then
    echo -e "${RED}Error: Not in the root directory of a git repository.${NC}"
    exit 1
fi

# Create hooks directory if it doesn't exist
mkdir -p .git/hooks

# Copy the pre-commit hook
cp pre-commit .git/hooks/
chmod +x .git/hooks/pre-commit

echo -e "${GREEN}Pre-commit hook installed successfully.${NC}"

# Check if fourmolu is installed
if ! command -v fourmolu &> /dev/null; then
    echo -e "${YELLOW}Warning: fourmolu is not installed.${NC}"
    echo -e "To install it, run: ${BLUE}stack install fourmolu${NC}"
fi

echo -e "${GREEN}Done!${NC}"
