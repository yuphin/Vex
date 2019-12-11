#pragma once
#include <string>
#include <map>
#include "parser.h"

// Give Flex the prototype of yylex we want ...
# define YY_DECL \
  yy::parser::symbol_type yylex (driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

class driver {
public:
    driver();
    std::map<std::string, int> variables;
    int result;
    // Run the parser on file F.  Return 0 on success.
    int parse(const std::string& f);
    // The name of the file being parsed.
    std::string file;
    bool trace_parsing;
    // Scanner handling
    void scan_begin();
    void scan_end();
    // Whether to generate scanner debug traces.
    bool trace_scanning;
    // The token's location used by the scanner.
    yy::location location;
};
