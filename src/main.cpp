#include <iostream>
#include "Logger.h"
#include "AST.h"
#include "Driver.h"

int main(int argc, char* argv[]) {
   driver drv;
    for (int i = 1; i < argc; ++i)
        if (argv[i] == std::string("-p"))
            drv.trace_parsing = true;
        else if (argv[i] == std::string("-s"))
            drv.trace_scanning = true;
        else if (!drv.parse(argv[i]))
            std::cout << "Parsed!" << '\n';
        else
            std::cerr << "No file provided!" << '\n';
    
    drv.generate_code();
    return 0;
}