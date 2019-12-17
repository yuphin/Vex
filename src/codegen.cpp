#include "codegen.h"

llvm::Value* codegen::visit(NumAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(el.val));
}

llvm::Value* codegen::visit(AssignmentStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(ReturnStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(IfStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(ForStatementAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(WhileStatementAST& el) {
	return nullptr;
}

void codegen::print_IR() {
	the_module->print(llvm::errs(), nullptr);
}

llvm::Value* codegen::visit(BaseAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(3.14));
}

llvm::Value* codegen::visit(TopAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(VariableDeclAST& el) {
	llvm::Value* v = sym_tab[el.name];
	if (!v) {
		// To be replaced with logger
		return nullptr;
		std::cerr << "Unknown variable!\n";
	} 
	return v;
}

llvm::Value* codegen::visit(FunctionAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(FunctionDeclAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(FunctionBodyAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(ExprAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(BinaryExprAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(UnaryExprAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(VariableExprAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(InvocationAST& el) {
	return nullptr;
}

llvm::Value* codegen::visit(StatementAST& el) {
	return nullptr;
}
