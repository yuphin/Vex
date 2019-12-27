#pragma once
#include "Visitor.h"
#include "AST.h"

namespace Vex {
	struct ASTChecker : public Visitor {
		virtual llvm::Value* visit(BaseAST& el) override;
		virtual llvm::Value* visit(TopAST& el) override;
		virtual llvm::Value* visit(VariableDeclAST& el) override;
		virtual llvm::Value* visit(FunctionAST& el) override;
		virtual llvm::Value* visit(FunctionDeclAST& el) override;
		virtual llvm::Value* visit(FunctionBodyAST& el) override;
		virtual llvm::Value* visit(ExprAST& el) override;
		virtual llvm::Value* visit(BinaryExprAST& el) override;
		virtual llvm::Value* visit(UnaryExprAST& el) override;
		virtual llvm::Value* visit(VariableAST& el) override;
		virtual llvm::Value* visit(IntNumAST& el) override;
		virtual llvm::Value* visit(FloatingNumAST& el) override;
		virtual llvm::Value* visit(AssignmentStatementAST& el) override;
		virtual llvm::Value* visit(ReturnStatementAST& el) override;
		virtual llvm::Value* visit(PrintStatementAST& el) override;
		virtual llvm::Value* visit(ReadStatementAST& el) override;
		virtual llvm::Value* visit(IfStatementAST& el) override;
		virtual llvm::Value* visit(ForStatementAST& el) override;
		virtual llvm::Value* visit(WhileStatementAST& el) override;
		virtual llvm::Value* visit(InvocationAST& el) override;
		virtual llvm::Value* visit(StatementAST& el) override;
		virtual llvm::Value* visit(StatementBlockAST& el) override;
		bool get_err();
		private:
		bool ret_in_statement = false;
		bool err = false;
		bool in_func = false;
		std::unordered_map<std::string, Type*> sym_tab;
		std::unordered_map<std::string, Type*> global_tab;
		std::unordered_map<std::string, obj_type> func_tab;


	};

#define AST_ERROR(...) {VEX_ERROR(__VA_ARGS__); err=true;}

}