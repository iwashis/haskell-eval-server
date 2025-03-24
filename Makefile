.PHONY: install-hooks format lint test clean

# Install git hooks
install-hooks:
	@bash install-hooks.sh

# Format Haskell code with fourmolu
format:
	@echo "Formatting Haskell files..."
	@find app src test -name "*.hs" -exec fourmolu --mode inplace {} \;
	@echo "Formatting complete."

# Check format without modifying files
lint:
	@echo "Checking Haskell file formatting..."
	@find app src test -name "*.hs" -exec fourmolu --mode check {} \;

# Run tests
test:
	@stack test

# Clean build artifacts
clean:
	@stack clean

# Format and run tests
check: format test

# Default target
all: install-hooks format test
