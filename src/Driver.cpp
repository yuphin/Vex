#include "Driver.h"
#include "Parser.h"

driver::driver() : trace_parsing(false), trace_scanning(false) {
    global_context = std::make_unique<GlobalContext>();
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
    
    CodeGenVisitor cg("main", global_context.get());
    root->accept(cg);
    cg.print_IR();
   // cg.print_IR();
}