#pragma once

#include <memory>
#include <map>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include "visitor.h"
#include "ast.h"


struct codegen : public visitor {

	llvm::LLVMContext context;
	std::unique_ptr<llvm::Module> the_module;
	std::map<std::string, llvm::Value*> sym_tab;
	llvm::Value* value;

	codegen(const std::string& module_name) {
		the_module = std::make_unique<llvm::Module>(module_name,context);
		llvm::IRBuilder<> Builder(context);
	}
	 void print_IR();
	 virtual llvm::Value* visit(BaseAST& el) override;
	 virtual llvm::Value* visit(TopAST& el) override;
	 virtual llvm::Value* visit(VariableDeclAST& el) override;
	 virtual llvm::Value* visit(FunctionAST& el) override;
	 virtual llvm::Value* visit(FunctionDeclAST& el) override;
	 virtual llvm::Value* visit(FunctionBodyAST& el) override;
	 virtual llvm::Value* visit(ExprAST& el) override;
	 virtual llvm::Value* visit(BinaryExprAST& el) override;
	 virtual llvm::Value* visit(UnaryExprAST& el) override;
	 virtual llvm::Value* visit(VariableExprAST& el) override;
	 virtual llvm::Value* visit(NumAST& el) override;
	 virtual llvm::Value* visit(AssignmentStatementAST& el) override;
	 virtual llvm::Value* visit(ReturnStatementAST& el) override; 
	 virtual llvm::Value* visit(PrintStatementAST& el) override; 
	 virtual llvm::Value* visit(ReadStatementAST& el) override; 
	 virtual llvm::Value* visit(IfStatementAST& el) override; 
	 virtual llvm::Value* visit(ForStatementAST& el) override; 
	 virtual llvm::Value* visit(WhileStatementAST& el) override; 
	 virtual llvm::Value* visit(InvocationAST& el) override;
	 virtual llvm::Value* visit(StatementAST& el) override;
};