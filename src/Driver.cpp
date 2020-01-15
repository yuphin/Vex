#include "Driver.h"
#include "Parser.h"

using namespace Vex;
Driver::Driver() : trace_parsing(false), trace_scanning(false) {
	global_context = std::make_unique<GlobalContext>();
}

bool Driver::fexists(const char* filename) {
	std::ifstream ifile(filename);
	return ifile.good();
}

void Driver::print_help() {

	std::string help_text = R"(Vex - V compiler
Usage : vex [options] file
Options:
	-O[0-3]			Sets the optimization level, the default is -O0 and can be set up to -O3
	-emit-ir		Emits LLVM-IR code
	-help			Prints this message and exits
	-o [filename]		Emits executable with given name
	-emit-ast		Pretty prints AST
	-p			Enables debug mode during parsing
	-s			Enables debug mode during scanning
	-emit-oc		Emits object code(named either output.o or the argument specified with -o)
			)";
	Logger::set_printer_mode();
	VEX_TRACE("{0}", help_text);
	exit(0);

}

void Driver::parse_args(int argc, char* argv[]) {
	std::regex opts("-O[0-3]");
	std::regex fn("(.*).v");
	std::string input_name = "";
	if (!argc) {
		VEX_ERROR("No input files");
		print_help();
	}
	for (int i = 1; i < argc; ++i) {
		if (argv[i] == std::string("-p")) {
			trace_parsing = true;
		} else if (argv[i] == std::string("-s")) {
			trace_scanning = true;
		} else if (std::regex_match(argv[i], opts)) {
			opt_level = static_cast<int>(argv[i][2] - '0');
		} else if (argv[i] == std::string("-o")) {
			VEX_ASSERT(argv[i + 1], "Invalid argument");
			output_name = argv[i + 1];
		} else if (argv[i] == std::string("-emit-ir")) {
			emit_ir = true;
		} else if (argv[i] == std::string("-emit-oc")) {
			emit_oc = true;
		} else if (argv[i] == std::string("-emit-ast")) {
			emit_ast = true;
		} else if (std::regex_match(argv[i], fn)) {
			input_name = argv[i];
		} else if (argv[i] == std::string("-help") || argv[i] == std::string("-h")) {
			print_help();
		}
	}
	VEX_ASSERT(fexists(input_name.c_str()), "File doesn't exist!");

	VEX_ASSERT(!parse(input_name), "Parsing failed");
}

int Driver::parse(const std::string& f) {
	file = f;
	location.initialize(&file);
	scan_begin();
	yy::parser parse(*this);
	parse.set_debug_level(trace_parsing);
	int res = parse();
	scan_end();
	return res;
}

void Driver::generate_code() {
	if (!result) {
		CodeGen cg("main", global_context.get(), opt_level);
		ASTChecker ac;
		root->accept(ac);
		if (!ac.get_err()) {
			root->accept(cg);
			if (emit_ir) {
				cg.emit_IR();
			}
			if (emit_oc) {
				output_name != "" ?
					cg.emit_object_code(output_name + ".o") :
					cg.emit_object_code();
			} else if (emit_ast) {
				ASTPrinter ap;
				root->accept(ap);
			} else {
				output_name != "" ?
					cg.emit_executable(output_name) :
					cg.emit_executable();
			}

		}


	}
}

