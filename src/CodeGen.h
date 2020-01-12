#pragma once
#include "Visitor.h"
#include "AST.h"

namespace Vex {
	struct CodeGen : public Visitor<llvm::Value*> {

		llvm::LLVMContext context;
		std::unique_ptr<llvm::Module> curr_module;
		std::unique_ptr<llvm::IRBuilder<>> Builder;
		std::unique_ptr<llvm::legacy::FunctionPassManager> fpm;
		llvm::legacy::PassManager mpm;
		GlobalContext* unit_context;
		llvm::FunctionCallee print, read;
		CodeGen(const std::string& module_name, GlobalContext* unit_context,
			int opt_level = 0);
		void emit_IR();
		void emit_object_code(const std::string& filename = "output.o");
		void emit_executable(const std::string& filename = "a");
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
		// Utility functions

		// Gets the underlying type of an AllocaInst, GlobalValue or optionally a pointer type 
		llvm::Type* get_type(llvm::Value* V, bool get_ptr_type);

		// Gets the underlying type of an AllocaInst, GlobalValue, Pointer or a VectorTy
		// This calls 'get_type(...)' internally
		std::pair<llvm::Type*, llvm::Type*> get_underlying_type(llvm::Value*, llvm::Value*);
		std::pair<llvm::Type*, llvm::Type*> get_underlying_type(llvm::Type*, llvm::Value*);
		llvm::Type* get_underlying_type(llvm::Value*);
		// Gets the element pointer of a vector
		llvm::Value* get_addr(llvm::Value* v, int index);
		// Symbol lookup from the symbol table
		llvm::Value* symbol_lookup(const std::string& name);
		// Returns an LLVM type according to some V type
		llvm::Type* lookup_type(const Type& type);
		llvm::Type* lookup_type(int type);
		// Create a binary op instruction 
		llvm::Value* create_binary(llvm::Value* LHS, llvm::Value* RHS, int op, const llvm::Twine&);
		// Cast values according to a larger type
		std::pair<llvm::Value*, llvm::Value*> cast_values(llvm::Value* LHS, llvm::Value* RHS);
		// Create an alloca instruction(a stack variable) to the top of the function
		llvm::AllocaInst* insert_alloca_to_top(llvm::Function* func,
			const std::string& var_name, llvm::Type* type);
		llvm::Value* cast_according_to(llvm::Value* LHS, llvm::Value* RHS);
		// l_type and RHS types might differ in their types(which is the case for this func)
		// It's up to caller's responsibility to supply the correct types in this func
		llvm::Value* cast_according_to_t(llvm::Type* l_type, llvm::Value* RHS);
		// Check if the RHS type should be casted without having to load from memory
		bool should_cast(llvm::Type* l_type, llvm::Value* RHS);
		llvm::Constant* prepare_io(const std::string& str);
		// Utility method for creating constant int value
		llvm::Value* create_int(const int& val, bool should_decrement);

	};


}
