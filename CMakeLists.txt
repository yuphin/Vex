# CMakeList.txt : CMake project for vex, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

project ("vex")
find_package(BISON 3.4)
find_package(FLEX 2.6.4)
find_package(LLVM 9 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Current Source Dir:${CMAKE_CURRENT_SOURCE_DIR}/src")

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/lib/spdlog/include)
add_definitions(${LLVM_DEFINITIONS})
set(SRC_FILES
	    src/Logger.cpp
        src/main.cpp
        src/CodeGen.cpp
        src/ASTChecker.cpp
        src/ASTPayload.cpp
        src/ASTPrinter.cpp
        src/Parser.y
        src/Scanner.l
        src/Driver.cpp)
set(HDR_FILES
	    src/Logger.h
        src/Context.h
        src/AST.h
        src/Visitor.h
        src/CodeGen.h
        src/ASTPayload.h
        src/ASTPrinter.h
        src/ASTChecker.h
        src/Driver.h)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG")


# Additional cleanup
set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/location.hh" "${CMAKE_CURRENT_SOURCE_DIR}/src/parser.output"  )

flex_target(Scanner src/Scanner.l
       ${CMAKE_CURRENT_SOURCE_DIR}/src/Scanner.cpp
        COMPILE_FLAGS)
bison_target(Parser src/Parser.y
       ${CMAKE_CURRENT_SOURCE_DIR}/src/Parser.cpp
        DEFINES_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/Parser.h
        COMPILE_FLAGS "-v")
add_flex_bison_dependency(Scanner Parser)

# Add source to this project's executable.
add_executable (vex ${SRC_FILES} ${HDR_FILES} ${FLEX_Scanner_OUTPUTS} ${BISON_Parser_OUTPUTS})

llvm_map_components_to_libnames(llvm_libs support core irreader all)

# Link against LLVM libraries
target_link_libraries(vex ${llvm_libs})


