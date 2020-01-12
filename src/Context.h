#pragma once
#include "Logger.h"
#include <unordered_map>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <stack>
#include <tuple>
#include <sstream> 
#include <memory>
#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>


namespace Vex {

	// This header containts all required LLVM related headers and structs for
	// code generation

	// A function context is created upon entering a function
	struct FuncContext {
		std::unordered_map<std::string, llvm::AllocaInst*> sym_tab;
		llvm::AllocaInst* return_val = nullptr;
		llvm::BasicBlock* return_br = nullptr;
		bool in_params = false;

	};
	
	// A global context is unique to a translation unit
	struct GlobalContext {
		std::stack<FuncContext> call_stack;
		std::unordered_map<std::string, llvm::Value* > sym_tab;
		bool in_global_namespace = true;

		// TODO: Move these state values inside FuncContext since we may want to
		// compile asynchronously

		// Is in statement?
		bool in_statement = false;
		// Are we evaluation an l-value?
		bool lhs_eval = false;
		// Are we invoking a function? If so set arg_type(See CodeGen::InvocationAST)
		bool invoke_func = false;
		llvm::Type* func_arg_type;

	};

	

}