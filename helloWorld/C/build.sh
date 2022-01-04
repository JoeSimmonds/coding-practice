pushd .

echo "Cleaning build artifacts"
rm -rf build

echo "Building Hello world App"
mkdir build
cd build
cmake ../
cmake --build .

echo "Running tests"
./HelloWorldSpecs

popd