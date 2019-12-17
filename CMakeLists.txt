# CMakeList.txt : CMake project for VexLLVM, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

project ("VexLLVM")

find_package(BISON)
find_package(FLEX)
find_package(LLVM 9 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Current Source Dir:${CMAKE_CURRENT_SOURCE_DIR}/src")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
set(SRC_FILES 
        src/main.cpp
        src/codegen.cpp
        src/parser.y
        src/scanner.l
        src/driver.cpp)
set(HDR_FILES
        src/ast.h
        src/visitor.h
        src/codegen.h
        src/driver.h)

# Additional cleanup
set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/location.hh" "${CMAKE_CURRENT_SOURCE_DIR}/src/parser.output"  )

flex_target(Scanner src/scanner.l
       ${CMAKE_CURRENT_SOURCE_DIR}/src/scanner.cpp
        COMPILE_FLAGS)
bison_target(Parser src/parser.y
       ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.cpp
        DEFINES_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.h
        COMPILE_FLAGS "-v")
add_flex_bison_dependency(Scanner Parser)
# Add source to this project's executable.
add_executable (VexLLVM ${SRC_FILES} ${HDR_FILES} ${FLEX_Scanner_OUTPUTS} ${BISON_Parser_OUTPUTS})

llvm_map_components_to_libnames(llvm_libs support core irreader)
# Link against LLVM libraries
target_link_libraries(VexLLVM ${llvm_libs})


