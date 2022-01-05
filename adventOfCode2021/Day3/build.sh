echo "Cleaning build artifacts"
rm -rf build

echo "Building Day 3 App"
cmake -B build
cmake --build build

echo "Running tests"
build/Day3Specs
