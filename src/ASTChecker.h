#pragma once
#include "Visitor.h"
#include "AST.h"


namespace Vex {


	struct ASTPayload;

	struct ASTChecker : public Visitor<std::unique_ptr<ASTPayload>> {
		virtual std::unique_ptr<ASTPayload> visit(BaseAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(TopAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(VariableDeclAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(FunctionAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(FunctionDeclAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(FunctionBodyAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(ExprAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(BinaryExprAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(UnaryExprAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(VariableAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(IntNumAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(FloatingNumAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(AssignmentStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(ReturnStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(PrintStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(ReadStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(IfStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(ForStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(WhileStatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(InvocationAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(StatementAST& el) override;
		virtual std::unique_ptr<ASTPayload> visit(StatementBlockAST& el) override;
		bool get_err();
		private:
		bool in_func = false;
		bool ret_in_statement = false;
		bool is_inner_stmt_block = false;
		bool err = false;
		obj_type func_type;
		std::string func_name;
		std::unordered_map<std::string, Type*> sym_tab;
		std::unordered_map<std::string, Type*> global_tab;
		std::unordered_map<std::string, obj_type> func_tab;
		Type* find_sym(const std::string&);


	};

#define AST_ERROR(...) {VEX_ERROR(__VA_ARGS__); err=true;}

}