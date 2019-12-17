#include "codegen.h"

llvm::Value* CodeGen::visit(NumAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(el.val));
}

llvm::Value* CodeGen::visit(AssignmentStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(ReturnStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(IfStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(ForStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(WhileStatementAST& el) {
	return nullptr;
}

void CodeGen::print_IR() {
	the_module->print(llvm::errs(), nullptr);
}

llvm::Value* CodeGen::visit(BaseAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(3.14));
}

llvm::Value* CodeGen::visit(TopAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(VariableDeclAST& el) {
	llvm::Value* v = sym_tab[el.name];
	if (!v) {
		// To be replaced with logger
		return nullptr;
		std::cerr << "Unknown variable!\n";
	} 
	return v;
}

llvm::Value* CodeGen::visit(FunctionAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(FunctionDeclAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(FunctionBodyAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(ExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(BinaryExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(UnaryExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(VariableExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(InvocationAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(StatementAST& el) {
	return nullptr;
}
