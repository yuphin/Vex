#pragma once
#include "Visitor.h"
#include "AST.h"


struct CodeGenVisitor : public Visitor {

	llvm::LLVMContext context;
	std::unique_ptr<llvm::Module> curr_module;
	std::unique_ptr<llvm::IRBuilder<>> Builder;
	std::unique_ptr<llvm::legacy::FunctionPassManager> fpm;
	llvm::legacy::PassManager mpm;
	GlobalContext* unit_context;

	CodeGenVisitor(const std::string& module_name, GlobalContext* unit_context) :
		unit_context(unit_context) {
		curr_module = std::make_unique<llvm::Module>(module_name, context);
		Builder = std::make_unique<llvm::IRBuilder<>>(context);
		fpm = std::make_unique<llvm::legacy::FunctionPassManager>(curr_module.get());
		llvm::PassManagerBuilder pmbuilder;
		pmbuilder.OptLevel = 0;
		pmbuilder.populateFunctionPassManager(*fpm);
		pmbuilder.populateModulePassManager(mpm);
		/*
		// Create a new pass manager attached to it.
		

		// Promote allocas to registers.
		fpm->add(llvm::createPromoteMemoryToRegisterPass());
		// Do simple "peephole" optimizations and bit-twiddling optzns.
		fpm->add(llvm::createInstructionCombiningPass());
		// Reassociate expressions.
		fpm->add(llvm::createReassociatePass());
		// Eliminate Common SubExpressions.
		fpm->add(llvm::createGVNPass());
		// Simplify the control flow graph (deleting unreachable blocks, etc).
		fpm->add(llvm::createCFGSimplificationPass());

		fpm->add(llvm::createDeadCodeEliminationPass());
		fpm->add(llvm::createDeadInstEliminationPass());
		fpm->add(llvm::createDeadInstEliminationPass());
		

		fpm->doInitialization();
		*/
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
	llvm::Type* get_type(llvm::Value* V,bool);
	llvm::Value* create_binary(llvm::Value* LHS, llvm::Value* RHS, int op,const llvm::Twine&);
	std::pair<llvm::Value*, llvm::Value*> cast_values(llvm::Value* LHS, llvm::Value* RHS);
	llvm::Value* create_cmp(llvm::Value* LHS, llvm::Value* RHS, llvm::CmpInst::Predicate P, const llvm::Twine& name = "");
	llvm::AllocaInst* insert_alloca_to_top(llvm::Function* func,
		const std::string& var_name, llvm::Type* type);
	llvm::Value* cast_according_to(llvm::Value* LHS, llvm::Value* RHS);
	llvm::Value* cast_according_to_t(llvm::Type* l_type, llvm::Value* RHS);

};

