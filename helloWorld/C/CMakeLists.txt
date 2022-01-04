cmake_minimum_required(VERSION 3.22)

# set the project name
project(HelloWorld VERSION 1.0)

## add the unity library
add_subdirectory("Unity")

# Library
add_library(HelloWorld "helloWorld.c")

# Specs
add_executable(HelloWorldSpecs specs.c)

target_include_directories(HelloWorldSpecs PUBLIC 
                            "${PROJECT_BINARY_DIR}"
                            "${PROJECT_SOURCE_DIR}/Unity/src"
                          )

target_link_libraries(HelloWorldSpecs PUBLIC "unity" "HelloWorld")

# Application
add_executable(HelloWorldApp "helloWorldApp.c")


target_link_libraries(HelloWorldApp PUBLIC "HelloWorld")

target_include_directories(HelloWorldApp PUBLIC 
                            "${PROJECT_BINARY_DIR}"
                          )

