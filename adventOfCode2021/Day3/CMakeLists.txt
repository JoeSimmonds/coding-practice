cmake_minimum_required(VERSION 3.22)

# set the project name
project(Day3 VERSION 1.0)

add_subdirectory("Unity")

# Library
add_library(Day3 "lib/Day3.c")

# Specs
add_executable(Day3Specs specs/Day3Specs.c)
target_include_directories(Day3Specs PUBLIC 
                            "${PROJECT_SOURCE_DIR}/Unity/src"
                            "${PROJECT_SOURCE_DIR}/lib"
                          )
target_link_libraries(Day3Specs PUBLIC "unity" "Day3")

# Application
add_executable(Day3App "app/Day3App.c")
target_include_directories(Day3App PUBLIC "${PROJECT_SOURCE_DIR}/lib")
target_link_libraries(Day3App PUBLIC "Day3")