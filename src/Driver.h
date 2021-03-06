#pragma once
#include <string>
#include <regex>
#include "Parser.h"
#include <fstream> 
#include "Context.h"

// Give Flex the prototype of yylex we want ...
# define YY_DECL \
  yy::parser::symbol_type yylex (Driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

using namespace Vex;
class Driver {
	public:

	Driver();
	std::map<std::string, int> variables;
	std::unique_ptr<BaseAST> root;
	std::unique_ptr<GlobalContext> global_context;
	int result = 0;

	void parse_args(int argc,char* argv[]);
	// Run the parser on file F.  Return 0 on success.
	int parse(const std::string& f);
	// The name of the file being parsed.
	std::string file;
	bool trace_parsing;
	// Scanner handling
	void scan_begin();
	void scan_end();
	void generate_code();
	// Whether to generate scanner debug traces.
	bool trace_scanning;
	// The token's location used by the scanner.
	yy::location location;

	private:
	void print_help();
	bool fexists(const char*);
	int opt_level = 0;
	bool emit_ir = false;
	bool emit_oc = false;
	bool emit_ast = false;
	std::string output_name = "";
};
