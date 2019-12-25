#include "ASTChecker.h"

llvm::Value* ASTChecker::visit(BaseAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(TopAST& el) {
	for (auto& dl : el.declaration_list) {
		dl->accept(*this);
	}
	for (auto& fl : el.function_list) {
		fl->accept(*this);

	}
	return nullptr;
}

llvm::Value* ASTChecker::visit(VariableDeclAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(FunctionAST& el) {
	el.prototype->accept(*this);
	el.body->accept(*this);
	return nullptr;
}

llvm::Value* ASTChecker::visit(FunctionDeclAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(FunctionBodyAST& el) {
	for (auto& dl : el.declaration_list) {
		dl->accept(*this);
	}

	el.statement_block->accept(*this);
	return nullptr;
}

llvm::Value* ASTChecker::visit(ExprAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(BinaryExprAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(UnaryExprAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(VariableAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(IntNumAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(FloatingNumAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(AssignmentStatementAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(ReturnStatementAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(IfStatementAST& el) {
	el.if_expr->accept(*this);

	auto then_has_return = false;
	for (auto& expr : el.then_blk->statement_list) {
		if (dynamic_cast<ReturnStatementAST*>(expr.get())) {
			then_has_return = true;
			break;
		}
	}
	el.then_blk->accept(*this);
	if (el.else_blk) {
		el.else_blk->accept(*this);
		for (auto& expr : el.else_blk->statement_list) {
			if (then_has_return && dynamic_cast<ReturnStatementAST*>(expr.get())) {
				this->ret_in_statement = true;
			}
		}
	}
	return nullptr;
}

llvm::Value* ASTChecker::visit(ForStatementAST& el) {
	el.assign_statement->accept(*this);
	el.to_expr->accept(*this); 
	if(el.by_expr)
		el.by_expr->accept(*this);
	el.statement_block->accept(*this);
	return nullptr;
}

llvm::Value* ASTChecker::visit(WhileStatementAST& el) {
	el.while_expr->accept(*this);
	el.statement_block->accept(*this);
	return nullptr;
}

llvm::Value* ASTChecker::visit(InvocationAST& el) {
	for (auto& arg : el.args) {
		arg->accept(*this);
	}
	return nullptr;
}

llvm::Value* ASTChecker::visit(StatementAST& el) {
	return nullptr;
}

llvm::Value* ASTChecker::visit(StatementBlockAST& el) {
	auto has_ret = false;
	auto it = el.statement_list.begin();
	while (it != el.statement_list.end()) {
		if (has_ret || ret_in_statement) {
			it = el.statement_list.erase(it);
			continue;
		} else {
			it->get()->accept(*this);
		}
		if (!has_ret && dynamic_cast<ReturnStatementAST*>(it->get())) {
			it->get()->accept(*this);
			has_ret = true;
		}
		it++;

	}
	this->ret_in_statement = false;
	return nullptr;
}
