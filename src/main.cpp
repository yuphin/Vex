#include <iostream>
#include "Logger.h"
#include "AST.h"
#include "Driver.h"

int main(int argc, char* argv[]) {
	Vex::Logger::init();
	Driver drv;
	drv.parse_args(argc, argv);

	drv.generate_code();

	return 0;
}