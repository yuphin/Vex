#pragma once
#include "Visitor.h"
#include "AST.h"


namespace Vex {
	struct ASTPrinter : public Visitor<void> {
		// Inherited via Visitor
		virtual void visit(BaseAST& el) override;
		virtual void visit(TopAST& el) override;
		virtual void visit(VariableDeclAST& el) override;
		virtual void visit(FunctionAST& el) override;
		virtual void visit(FunctionDeclAST& el) override;
		virtual void visit(FunctionBodyAST& el) override;
		virtual void visit(ExprAST& el) override;
		virtual void visit(BinaryExprAST& el) override;
		virtual void visit(UnaryExprAST& el) override;
		virtual void visit(VariableAST& el) override;
		virtual void visit(IntNumAST& el) override;
		virtual void visit(FloatingNumAST& el) override;
		virtual void visit(InvocationAST& el) override;
		virtual void visit(StatementBlockAST& el) override;
		virtual void visit(StatementAST& el) override;
		virtual void visit(AssignmentStatementAST& el) override;
		virtual void visit(ReturnStatementAST& el) override;
		virtual void visit(PrintStatementAST& el) override;
		virtual void visit(ReadStatementAST& el) override;
		virtual void visit(IfStatementAST& el) override;
		virtual void visit(ForStatementAST& el) override;
		virtual void visit(WhileStatementAST& el) override;

		private:
		int indent_counter = 0;
		std::string get_alignment(int indent);
		std::string print_var_type(Type*);
		std::string print_var_type(obj_type);
		std::string print_var_type(un_op);
	};

}