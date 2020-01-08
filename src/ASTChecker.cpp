#include "ASTChecker.h"
namespace Vex {
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
		VEX_ASSERT(func_tab.count("main") == 1, "Function \"main\" doesn't exist!");
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(VariableDeclAST& el) {

		if (!in_func && el.var_type->is_array && !*el.var_type->array_size) {
			AST_ERROR("Vector type cannot be empty outside the function : {0}", el.location);

		}

		if ((!in_func && global_tab[el.name]) || (in_func && sym_tab[el.name])) {
			AST_ERROR("Redeclaration of variable \"{0}\" : {1}", el.name, el.location);

		} else {
			(in_func ? sym_tab[el.name] : global_tab[el.name]) = el.var_type.get();
		}
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(FunctionAST& el) {
		in_func = true;
		el.prototype->accept(*this);
		if (!err) {
			el.body->accept(*this);
		}
		in_func = false;
		sym_tab.clear();
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(FunctionDeclAST& el) {
		if (func_tab.count(el.name)) {
			AST_ERROR("Redeclaration of function \"{0}\" : {1}", el.name, el.location);
		} else {
			func_tab[el.name] = el.func_type;

		}
		for (auto& decl : el.parameter_list) {
			decl->accept(*this);
		}
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
		if (!sym_tab[el.name] && !global_tab[el.name]) {
			AST_ERROR("Unknown variable name \"{0}\" : {1}", el.name, el.location);
		} else if (el.indexExpr) {
			if (auto int_expr = dynamic_cast<IntNumAST*>(el.indexExpr.get())) {
				if (int_expr->val < 1) {
					AST_ERROR("Index expr is <1 : {0}", el.location);
				} else if (int_expr->val >= *sym_tab[el.name]->array_size) {
					AST_ERROR("Array index out of range : {0}", el.location);
				}
			} else {
				AST_ERROR("Index cannot be other than unsigned integer for \"{0}\": {1}", el.name, el.location);
			}
		}
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(IntNumAST& el) {
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(FloatingNumAST& el) {
		return nullptr;
	}

	llvm::Value* ASTChecker::visit(AssignmentStatementAST& el) {
		el.lvalue->accept(*this);
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
		if (el.by_expr)
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
		if (!func_tab.count(el.callee)) {
			AST_ERROR("Unknown func name \"{0}\" : {1}", el.callee, el.location);
		} else {
			for (auto& arg : el.args) {
				arg->accept(*this);
			}
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

	bool ASTChecker::get_err() {
		return err;
	}
}
