#include "Driver.h"
#include "Parser.h"

using namespace Vex;
driver::driver() : trace_parsing(false), trace_scanning(false) {
	global_context = std::make_unique<GlobalContext>();
}

void driver::parse_args(int argc, char* argv[]) {
	std::regex opts("-O[0-3]");
	std::regex fn("(.*).v");
	std::string input_name = "";
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
		} else if (std::regex_match(argv[i], fn)) {
			input_name = argv[i];
		} else {
			VEX_ASSERT(input_name != "", "No input files");
		}
	}
	VEX_ASSERT(!parse(input_name), "Parsing failed");
}

int driver::parse(const std::string& f) {
	file = f;
	location.initialize(&file);
	scan_begin();
	yy::parser parse(*this);
	parse.set_debug_level(trace_parsing);
	int res = parse();
	scan_end();
	return res;
}

void driver::generate_code() {
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
			} else {
				output_name != "" ?
					cg.emit_executable(output_name) :
					cg.emit_executable();
			}
			
		}


	}
}

