#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include "location.hh"
#include "ASTPayload.h"
#include "CodeGen.h"
#include "ASTChecker.h"
#include "ASTPrinter.h"

#define ACCEPT(X) virtual X accept(Visitor<X>& v) override {return v.visit(*this);}
#define ACCEPT_Z(X) virtual X accept(Visitor<X>&v) = 0;
#define GENERATE_ACCEPTORS() ACCEPT(llvm::Value*) \
							 ACCEPT(std::unique_ptr<ASTPayload>) \
							 ACCEPT(void)

#define GENERATE_ABSTRCT_FUNCS() ACCEPT_Z(llvm::Value*) \
								 ACCEPT_Z(std::unique_ptr<ASTPayload>) \
								 ACCEPT_Z(void)
namespace Vex {
	struct ASTPayload;
	// Type structure for holding variable types
	struct Type {

		obj_type s_type;
		std::unique_ptr<unsigned int> array_size;
		bool is_array;

		explicit Type(obj_type s_type, std::unique_ptr<unsigned int> array_size) :
			s_type(s_type), array_size(std::move(array_size)), is_array(*this->array_size >= 0) {}

		explicit Type(obj_type s_type) : s_type(s_type), array_size(nullptr), is_array(false) {}


	};


	struct AST {
		GENERATE_ABSTRCT_FUNCS()

	};

	struct BaseAST : public  AST {

		yy::location location;

		virtual ~BaseAST() {}
		BaseAST() {}
		BaseAST(yy::location location) : location(location) {}
		GENERATE_ACCEPTORS()

	};


	// Base Expr node. Note that every expression has a value.
	// Currently this value is set to double regardless of the type.
	struct ExprAST : public BaseAST {

		char val;
		ExprAST(yy::location& location) : BaseAST(location) {}
		ExprAST(yy::location& location, const double& val) : BaseAST(location), val(val) {}
		ExprAST() {}
		GENERATE_ACCEPTORS()
	};

	struct BinaryExprAST : public ExprAST {
		bin_op binop;
		std::unique_ptr<ExprAST> LHS, RHS;

		BinaryExprAST(std::unique_ptr<ExprAST> LHS, bin_op binop,
			std::unique_ptr<ExprAST> RHS, yy::location& location)
			: ExprAST(location),
			binop(binop), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		GENERATE_ACCEPTORS()
	};

	struct UnaryExprAST : public ExprAST {
		un_op unop;
		std::unique_ptr<ExprAST> LHS;

		UnaryExprAST(un_op unop, std::unique_ptr<ExprAST> LHS, yy::location& location)
			: ExprAST(location), unop(unop), LHS(std::move(LHS)) {}
		GENERATE_ACCEPTORS()
	};

	struct VariableAST : public ExprAST {
		std::string name;
		std::unique_ptr<ExprAST> indexExpr;

		VariableAST(std::string&& name, yy::location& location,
			std::unique_ptr<ExprAST> indexExpr) :
			ExprAST(location), name(std::move(name)), indexExpr(std::move(indexExpr)) {}
		VariableAST(std::string&& name, yy::location& location) :
			ExprAST(location), name(std::move(name)), indexExpr(nullptr) {}
		GENERATE_ACCEPTORS()
	};


	// For number representation
	struct IntNumAST : public ExprAST {

		unsigned int val;
		IntNumAST(int val, yy::location& location) :
			ExprAST(location), val(val) {
#ifdef DEBUG
			std::cout << "Integer val is: " << this->val << std::endl;
#endif
	}
		IntNumAST(int val) : val(val) {}
		GENERATE_ACCEPTORS()
};

	struct FloatingNumAST : public ExprAST {


		double val;
		FloatingNumAST(double val, yy::location& location) :
			ExprAST(location), val(val) {
#ifdef DEBUG
			std::cout << "Floating val is: " << this->val << std::endl;
#endif
	}
		FloatingNumAST(double val) : val(val) {}
		GENERATE_ACCEPTORS()
	};

	// Base statement node
	struct StatementAST : public BaseAST {
		GENERATE_ACCEPTORS()
	};


	// For function calls
	struct InvocationAST : public StatementAST, public ExprAST {
		std::string callee;
		std::vector<std::unique_ptr<ExprAST>> args;
		yy::location location;

		InvocationAST(std::string&& callee, 
			std::vector<std::unique_ptr<ExprAST>> args, const yy::location& loc)
			: callee(std::move(callee)), args(std::move(args)), location(loc) {}
		GENERATE_ACCEPTORS()
	};


	struct AssignmentStatementAST : public StatementAST {
		std::unique_ptr<VariableAST> lvalue;
		std::unique_ptr<ExprAST> expr;

		AssignmentStatementAST(std::unique_ptr<VariableAST> lvalue, std::unique_ptr<ExprAST> expr) :
			lvalue(std::move(lvalue)), expr(std::move(expr)) {}
		GENERATE_ACCEPTORS()
	};
	struct ReturnStatementAST : public StatementAST {
		std::unique_ptr<ExprAST> expr;
		ReturnStatementAST(std::unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}
		GENERATE_ACCEPTORS()
	};

	struct PrintStatementAST : public StatementAST {
		std::vector<std::unique_ptr<ExprAST>> print_exprs;

		PrintStatementAST(std::vector<std::unique_ptr<ExprAST>> print_exprs) :
			print_exprs(std::move(print_exprs)) {}
		GENERATE_ACCEPTORS()
	};

	struct ReadStatementAST : public StatementAST {
		std::vector<std::unique_ptr<VariableAST>> read_exprs;

		ReadStatementAST(std::vector<std::unique_ptr<VariableAST>> read_exprs) :
			read_exprs(std::move(read_exprs)) {}
		GENERATE_ACCEPTORS()
	};

	struct ForStatementAST : public StatementAST {
		// TODO: Make this lists of assignment later
		std::unique_ptr<AssignmentStatementAST> assign_statement;
		std::unique_ptr<ExprAST> to_expr, by_expr;
		std::unique_ptr<StatementBlockAST> statement_block;

		ForStatementAST(
			std::unique_ptr<AssignmentStatementAST> assign_statement,
			std::unique_ptr<ExprAST> to_expr,
			std::unique_ptr<StatementBlockAST> statement_block) :
			assign_statement(std::move(assign_statement)), to_expr(std::move(to_expr)),
			by_expr(nullptr), statement_block(std::move(statement_block)) {}
		ForStatementAST(
			std::unique_ptr<AssignmentStatementAST> assign_statement,
			std::unique_ptr<ExprAST> to_expr, std::unique_ptr<ExprAST> by_expr,
			std::unique_ptr<StatementBlockAST> statement_block) :
			assign_statement(std::move(assign_statement)), to_expr(std::move(to_expr)),
			by_expr(std::move(by_expr)), statement_block(std::move(statement_block)) {}
		GENERATE_ACCEPTORS()
	};

	struct IfStatementAST : public StatementAST {
		std::unique_ptr<ExprAST> if_expr;
		std::unique_ptr<StatementBlockAST> then_blk;
		std::unique_ptr<StatementBlockAST> else_blk = nullptr;

		IfStatementAST(std::unique_ptr<ExprAST> if_expr,
			std::unique_ptr<StatementBlockAST> then_blk) :
			if_expr(std::move(if_expr)), then_blk(std::move(then_blk)) {}
		IfStatementAST(
			std::unique_ptr<ExprAST> if_expr,
			std::unique_ptr<StatementBlockAST> then_blk,
			std::unique_ptr<StatementBlockAST> else_blk) :
			if_expr(std::move(if_expr)), then_blk(std::move(then_blk)), else_blk(std::move(else_blk)) {}
		GENERATE_ACCEPTORS()
	};

	struct WhileStatementAST : public StatementAST {
		std::unique_ptr<ExprAST> while_expr;
		std::unique_ptr<StatementBlockAST> statement_block;



		WhileStatementAST(std::unique_ptr<ExprAST> while_expr,
			std::unique_ptr<StatementBlockAST> statement_block) :
			while_expr(std::move(while_expr)), statement_block(std::move(statement_block)) {}

		GENERATE_ACCEPTORS()
	};
	struct VariableDeclAST : public BaseAST {
		std::unique_ptr<Type> var_type;
		std::string name;

		VariableDeclAST(std::string&& name, yy::location& location, std::unique_ptr<Type> var_type) :
			BaseAST(location), var_type(std::move(var_type)), name(std::move(name)) {}
		GENERATE_ACCEPTORS()
	};


	struct FunctionDeclAST : public BaseAST {

		obj_type func_type;
		std::string name;
		std::vector <std::unique_ptr<VariableDeclAST>> parameter_list;

		FunctionDeclAST(std::string&& name, yy::location& location,
			obj_type& func_type, std::vector <std::unique_ptr<VariableDeclAST>> parameter_list) :
			BaseAST(location), func_type(func_type),
			name(std::move(name)), parameter_list(std::move(parameter_list)) {}
		GENERATE_ACCEPTORS()
	};


	struct StatementBlockAST : public BaseAST {
		std::vector<std::unique_ptr<StatementAST>> statement_list;

		GENERATE_ACCEPTORS()

	};


	struct FunctionBodyAST : public BaseAST {

		std::vector<std::unique_ptr<VariableDeclAST>> declaration_list;
		std::unique_ptr<StatementBlockAST> statement_block;

		FunctionBodyAST(std::vector<std::unique_ptr<VariableDeclAST>> declaration_list
			, std::unique_ptr<StatementBlockAST> statement_block) :
			declaration_list(std::move(declaration_list)),
			statement_block(std::move(statement_block)) {}
		GENERATE_ACCEPTORS()
	};


	struct FunctionAST : public BaseAST {
		std::unique_ptr<FunctionDeclAST> prototype;
		std::unique_ptr<FunctionBodyAST> body;

		FunctionAST(std::unique_ptr<FunctionDeclAST> prototype, std::unique_ptr<FunctionBodyAST> body)
			: prototype(std::move(prototype)), body(std::move(body)) {}
		GENERATE_ACCEPTORS()
	};


	// Root node for the program. This will be the entry point.
	struct TopAST : public BaseAST {

		std::vector<std::unique_ptr<VariableDeclAST>> declaration_list;
		std::vector<std::unique_ptr<FunctionAST>> function_list;

		TopAST(std::vector<std::unique_ptr<VariableDeclAST>> declaration_list,
			std::vector<std::unique_ptr<FunctionAST>> function_list
		) : declaration_list(std::move(declaration_list)), function_list(std::move(function_list)) {}
		GENERATE_ACCEPTORS()
	};
}

