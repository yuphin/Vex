#pragma once
#include <unordered_map>
#include <stack>
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
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
struct FuncContext {
    std::unordered_map<std::string, llvm::AllocaInst*> sym_tab;
    llvm::AllocaInst* return_val = nullptr;
    llvm::BasicBlock* return_br = nullptr;

};
struct GlobalContext {
    std::stack<FuncContext> call_stack;
    std::unordered_map<std::string, llvm::Value* > sym_tab;
    bool in_global_namespace = true;
    bool in_statement = false;
    
};