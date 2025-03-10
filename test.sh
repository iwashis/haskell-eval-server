#!/bin/bash

# Create a temporary Haskell file using case expressions instead of pattern matching
cat > test_code.hs << 'EOF'
-- Factorial using case expression instead of pattern matching
factorial :: Integer -> Integer
factorial n = case n of
  0 -> 1
  _ -> n * factorial (n-1)

x = fmap factorial [1..10]

-- Main function to print factorial of 5
main = putStrLn $ "The factorial of 5 is: " ++ show x
EOF

echo "Testing with alternative factorial implementation..."
echo "----------------------------------------"
cat test_code.hs
echo "----------------------------------------"

# Use nc to send the file to the server
cat test_code.hs | nc localhost 3000

# Clean up
rm test_code.hs
