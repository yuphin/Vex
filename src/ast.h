#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include "location.hh"
#include "codegen.h"

// Variable type
enum obj_type { INT, REAL };

// Current binary operators
enum bin_op { EQ, LT, GT, LTE, GTE, AND, OR, NOT, ADD, SUB, MULT, DIV, MOD };

// Current unary operators
enum un_op { UNOT, MINUS };


// Type structure for holding variable types
struct Type {

	obj_type s_type;
	std::unique_ptr<int> array_size;
	bool is_array;

	Type(const obj_type& s_type, std::unique_ptr<int> array_size) :
		s_type(s_type), array_size(std::move(array_size)), is_array(*this->array_size >= 0) {}

	Type(const obj_type& s_type) : s_type(s_type), array_size(nullptr), is_array(false) {}
};


struct AST {
	virtual llvm::Value*  accept(Visitor& v) = 0;
};

struct BaseAST : public  AST {

	yy::location location;

	virtual ~BaseAST() {}
	BaseAST() {}
	BaseAST(yy::location location) : location(location) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}

};



// Base Expr node. Note that every expression has a value.
// Currently this value is set to double regardless of the type.
struct ExprAST : public BaseAST {

	double val;

	ExprAST(yy::location& location) : BaseAST(location) {}
	ExprAST(const double& val, yy::location& location) : BaseAST(location), val(val) {
		std::cout << "Read val:" << val << std::endl;
	}
	ExprAST() {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
		
	}
};

struct BinaryExprAST : public ExprAST {
	bin_op binop;
	std::unique_ptr<ExprAST> LHS, RHS;

	BinaryExprAST(std::unique_ptr<ExprAST> LHS, bin_op binop,
		std::unique_ptr<ExprAST> RHS, yy::location& location)
		: ExprAST(location),
		binop(binop), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct UnaryExprAST : public ExprAST {
	un_op unop;
	std::unique_ptr<ExprAST> LHS;

	UnaryExprAST(un_op unop, std::unique_ptr<ExprAST> LHS, yy::location& location)
		: ExprAST(location), unop(unop), LHS(std::move(LHS)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct VariableAST : public ExprAST {
	std::string name;
	std::unique_ptr<ExprAST> indexExpr;

	VariableAST(std::string&& name, yy::location& location,
		std::unique_ptr<ExprAST> indexExpr) :
		ExprAST(location), name(std::move(name)), indexExpr(std::move(indexExpr)) {}
	VariableAST(std::string&& name, yy::location& location) :
		ExprAST(location), name(std::move(name)), indexExpr(nullptr) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


// For number representation
struct NumAST : public ExprAST {


	NumAST(const double& val, yy::location& location) :
		ExprAST(val, location) {
		// std::cout << "Val is: " <<  this->val << std::endl;
	}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

// For function calls
struct InvocationAST : public ExprAST {
	std::string callee;
	std::vector<std::unique_ptr<ExprAST>> args;


	InvocationAST(std::string&& callee, std::vector<std::unique_ptr<ExprAST>> args)
		: callee(std::move(callee)), args(std::move(args)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

 // Base statement node
struct StatementAST : public BaseAST {
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


struct AssignmentStatementAST : public StatementAST {
	std::unique_ptr<VariableAST> lvalue;
	std::unique_ptr<ExprAST> expr;

	AssignmentStatementAST(std::unique_ptr<VariableAST> lvalue, std::unique_ptr<ExprAST> expr) :
		lvalue(std::move(lvalue)), expr(std::move(expr)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};
struct ReturnStatementAST : public StatementAST {
	std::unique_ptr<ExprAST> expr;
	ReturnStatementAST(std::unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct PrintStatementAST : public StatementAST {
	std::vector<std::unique_ptr<ExprAST>> print_exprs;

	PrintStatementAST(std::vector<std::unique_ptr<ExprAST>> print_exprs) :
		print_exprs(std::move(print_exprs)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct ReadStatementAST : public StatementAST {
	std::vector<std::unique_ptr<VariableAST>> read_exprs;

	ReadStatementAST(std::vector<std::unique_ptr<VariableAST>> read_exprs) :
		read_exprs(std::move(read_exprs)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct ForStatementAST : public StatementAST {
	// TODO: Make this lists of assignment later
	std::unique_ptr<AssignmentStatementAST> assign_statement;
	std::unique_ptr<ExprAST> to_expr, by_expr;
	std::vector<std::unique_ptr<StatementAST>> statement_list;

	ForStatementAST(
		std::unique_ptr<AssignmentStatementAST> assign_statement,
		std::unique_ptr<ExprAST> to_expr,
		std::vector<std::unique_ptr<StatementAST>> statement_list) :
		assign_statement(std::move(assign_statement)), to_expr(std::move(to_expr)),
		statement_list(std::move(statement_list)) {}
	ForStatementAST(
		std::unique_ptr<AssignmentStatementAST> assign_statement,
		std::unique_ptr<ExprAST> to_expr, std::unique_ptr<ExprAST> by_expr,
		std::vector<std::unique_ptr<StatementAST>> statement_list) :
		assign_statement(std::move(assign_statement)), to_expr(std::move(to_expr)),
		by_expr(std::move(by_expr)), statement_list(std::move(statement_list)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct IfStatementAST : public StatementAST {
	std::unique_ptr<ExprAST> if_expr;
	std::vector<std::unique_ptr<StatementAST>> then_lst;
	std::vector<std::unique_ptr<StatementAST>> else_lst;

	IfStatementAST(std::unique_ptr<ExprAST> if_expr,
		std::vector<std::unique_ptr<StatementAST>> then_lst) :
		if_expr(std::move(if_expr)), then_lst(std::move(then_lst)) { }
	IfStatementAST(
		std::unique_ptr<ExprAST> if_expr,
		std::vector<std::unique_ptr<StatementAST>> then_lst,
		std::vector<std::unique_ptr<StatementAST>> else_lst) :
		if_expr(std::move(if_expr)), then_lst(std::move(then_lst)), else_lst(std::move(else_lst)) { }
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};

struct WhileStatementAST : public StatementAST {
	std::unique_ptr<ExprAST> while_expr;
	std::vector<std::unique_ptr<StatementAST>> statement_list;


	
	WhileStatementAST(std::unique_ptr<ExprAST> while_expr,
		std::vector<std::unique_ptr<StatementAST>> statement_list) :
		while_expr(std::move(while_expr)), statement_list(std::move(statement_list)) {}

	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};
struct VariableDeclAST : public BaseAST {
	std::unique_ptr<Type> var_type;
	std::string name;

	VariableDeclAST(std::string&& name, yy::location& location, std::unique_ptr<Type> var_type) :
		BaseAST(location), var_type(std::move(var_type)), name(std::move(name)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


struct FunctionDeclAST : public BaseAST {

	obj_type func_type;
	std::string name;
	std::vector <std::unique_ptr<VariableDeclAST>> parameter_list;

	// Incomplete: Possible ref value in arg_names 
	FunctionDeclAST(std::string&& name, yy::location& location,
		obj_type& func_type, std::vector <std::unique_ptr<VariableDeclAST>> parameter_list) :
		BaseAST(location), func_type(func_type),
		name(std::move(name)), parameter_list(std::move(parameter_list)) {}
	virtual llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


struct FunctionBodyAST : public BaseAST {

	std::vector<std::unique_ptr<VariableDeclAST>> declaration_list;
	std::vector<std::unique_ptr<StatementAST>> statement_list;

	FunctionBodyAST(std::vector<std::unique_ptr<VariableDeclAST>> declaration_list
		, std::vector<std::unique_ptr<StatementAST>> statement_list) :
		declaration_list(std::move(declaration_list)),
		statement_list(std::move(statement_list)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


struct FunctionAST : public BaseAST {
	std::unique_ptr<FunctionDeclAST> prototype;
	std::unique_ptr<FunctionBodyAST> body;
	
	FunctionAST(std::unique_ptr<FunctionDeclAST> prototype, std::unique_ptr<FunctionBodyAST> body)
		: prototype(std::move(prototype)), body(std::move(body)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


// Root node for the program. This will be the entry point.
struct TopAST : public BaseAST {

	std::vector<std::unique_ptr<VariableDeclAST>> declaration_list;
	std::vector<std::unique_ptr<FunctionAST>> function_list;

	TopAST(std::vector<std::unique_ptr<VariableDeclAST>> declaration_list,
		std::vector<std::unique_ptr<FunctionAST>> function_list
	) : declaration_list(std::move(declaration_list)), function_list(std::move(function_list)) {}
	virtual  llvm::Value* accept(Visitor& v) override {
		return v.visit(*this);
	}
};


