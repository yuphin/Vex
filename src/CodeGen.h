#pragma once
#include "Visitor.h"
#include "AST.h"
#include "Context.h"

struct CodeGenVisitor : public Visitor {

	llvm::LLVMContext context;
	std::unique_ptr<llvm::Module> curr_module;
	std::unique_ptr<llvm::IRBuilder<>> Builder;
	GlobalContext* unit_context;

	CodeGenVisitor(const std::string& module_name, GlobalContext* unit_context) :
		unit_context(unit_context) {
		curr_module = std::make_unique<llvm::Module>(module_name, context);
		Builder = std::make_unique<llvm::IRBuilder<>>(context);
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

	private:
	llvm::Value* symbol_lookup(const std::string& name);
	llvm::Type* lookup_type(const Type& type);
	llvm::Type* lookup_type(int type);
	llvm::Value* create_cmp(llvm::Value* LHS, const llvm::Twine& name = "");
	llvm::AllocaInst* insert_alloca(llvm::Function* func,
		const std::string& var_name, llvm::Type* type);


};

