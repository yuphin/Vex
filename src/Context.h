#pragma once
#include <unordered_map>
#include <stack>
#include <llvm/IR/Value.h>
#include <memory>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/GlobalVariable.h>

struct FuncContext {
    std::unordered_map<std::string, llvm::Value*> sym_tab;

};
struct GlobalContext {
    std::stack<FuncContext> call_stack;
    std::unordered_map<std::string, llvm::Value* > sym_tab;
    bool in_global_namespace = true;
    
};