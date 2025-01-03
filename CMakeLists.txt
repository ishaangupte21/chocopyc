cmake_minimum_required(VERSION 3.26.0)
set(CMAKE_CXX_STANDARD 23)

# Setup cmake to export a compilation database.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(chocopyc CXX)

# C++23 Linker configuration to resolve versioning issues.
if (CMAKE_SYSTEM_NAME MATCHES "Darwin" 
    AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64"
    AND CMAKE_CXX_COMPILER_ID MATCHES "Clang"
    AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 18)
    # link_libraries(/opt/homebrew/cellar/llvm/19.1.3/lib/libunwind.1.0.dylib)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/opt/homebrew/cellar/llvm/19.1.3/lib/c++")
endif()

add_subdirectory(src)

add_executable(chocopyc main.cpp)

# Set the include paths
target_include_directories(chocopyc PUBLIC "${CMAKE_SOURCE_DIR}/include" "${CMAKE_BINARY_DIR}/include")
target_include_directories(chocopyc_parse PUBLIC "${CMAKE_SOURCE_DIR}/include" "${CMAKE_BINARY_DIR}/include")
target_include_directories(chocopyc_source PUBLIC "${CMAKE_SOURCE_DIR}/include" "${CMAKE_BINARY_DIR}/include")

# Set the link paths
target_link_libraries(chocopyc PUBLIC chocopyc_parse chocopyc_source)
