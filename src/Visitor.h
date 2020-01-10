#pragma once
#include "Context.h"

namespace Vex {
	// Variable type
	enum obj_type { INT = 0, REAL = 1 };

	// Current binary operators
	enum bin_op { EQ, NEQ, LT, GT, LTE, GTE, AND, OR, NOT, ADD, SUB, MULT, DIV, IDIV, MOD };

	// Current unary operators
	enum un_op { UNOT, MINUS };
	// Variable types
	struct Type;

	struct AST;
	struct BaseAST;
	struct TopAST;
	struct VariableDeclAST;
	struct FunctionAST;
	struct FunctionDeclAST;
	struct FunctionBodyAST;
	// Exprs
	struct ExprAST;
	struct BinaryExprAST;
	struct UnaryExprAST;
	struct VariableAST;
	struct IntNumAST;
	struct FloatingNumAST;
	struct InvocationAST;
	// Statements
	struct StatementBlockAST;
	struct StatementAST;
	struct AssignmentStatementAST;
	struct ReturnStatementAST;
	struct PrintStatementAST;
	struct ReadStatementAST;
	struct IfStatementAST;
	struct ForStatementAST;
	struct WhileStatementAST;


	template <class T = llvm::Value*>
	class Visitor {
		public:
		virtual T visit(BaseAST& el) = 0;
		virtual T visit(TopAST& el) = 0;
		virtual T visit(VariableDeclAST& el) = 0;
		virtual T visit(FunctionAST& el) = 0;
		virtual T visit(FunctionDeclAST& el) = 0;
		virtual T visit(FunctionBodyAST& el) = 0;
		virtual T visit(ExprAST& el) = 0;
		virtual T visit(BinaryExprAST& el) = 0;
		virtual T visit(UnaryExprAST& el) = 0;
		virtual T visit(VariableAST& el) = 0;
		virtual T visit(IntNumAST& el) = 0;
		virtual T visit(FloatingNumAST& el) = 0;
		virtual T visit(InvocationAST& el) = 0;
		virtual T visit(StatementBlockAST& el) = 0;
		virtual T visit(StatementAST& el) = 0;
		virtual T visit(AssignmentStatementAST& el) = 0;
		virtual T visit(ReturnStatementAST& el) = 0;
		virtual T visit(PrintStatementAST& el) = 0;
		virtual T visit(ReadStatementAST& el) = 0;
		virtual T visit(IfStatementAST& el) = 0;
		virtual T visit(ForStatementAST& el) = 0;
		virtual T visit(WhileStatementAST& el) = 0;
	};
}
