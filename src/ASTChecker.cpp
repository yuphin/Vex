#include "ASTChecker.h"
namespace Vex {
	std::unique_ptr<ASTPayload> ASTChecker::visit(BaseAST& el) {
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(TopAST& el) {
		for (auto& dl : el.declaration_list) {
			dl->accept(*this);
		}
		for (auto& fl : el.function_list) {
			fl->accept(*this);

		}
		VEX_ASSERT(func_tab.count("main") == 1, "Function \"main\" doesn't exist!");
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(VariableDeclAST& el) {

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

	std::unique_ptr<ASTPayload> ASTChecker::visit(FunctionAST& el) {
		in_func = true;
		el.prototype->accept(*this);
		if (!err) {
			el.body->accept(*this);
		}
		in_func = false;
		sym_tab.clear();
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(FunctionDeclAST& el) {
		this->func_type = el.func_type;
		this->func_name = el.name;
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

	std::unique_ptr<ASTPayload> ASTChecker::visit(FunctionBodyAST& el) {
		for (auto& dl : el.declaration_list) {
			dl->accept(*this);
		}

		el.statement_block->accept(*this);
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(ExprAST& el) {
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(BinaryExprAST& el) {
		auto LHS = el.LHS->accept(*this);
		auto RHS = el.RHS->accept(*this);

		return std::move(*LHS < *RHS ? LHS : RHS);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(UnaryExprAST& el) {
		return el.accept(*this);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(VariableAST& el) {
		std::unique_ptr<ASTPayload> ret_val = nullptr;
		if (auto var_type = find_sym(el.name)) {
			ret_val = std::make_unique<ASTPayload>(var_type,el.location);
		} else {
			AST_ERROR("Unknown variable name \"{0}\" : {1}", el.name, el.location);
			return nullptr;
		}

		if (el.indexExpr) {
			if (auto int_expr = dynamic_cast<IntNumAST*>(el.indexExpr.get())) {
				if (int_expr->val < 1) {
					AST_ERROR("Index expr is <1 : {0}", el.location);
				} else if (*sym_tab[el.name]->array_size && int_expr->val >= *sym_tab[el.name]->array_size) {
					AST_ERROR("Array index out of range : {0}", el.location);
				}
			} else {
				AST_ERROR("Index cannot be other than unsigned integer for \"{0}\": {1}", el.name, el.location);
			}

		
		}
		return ret_val;
	}

		std::unique_ptr<ASTPayload> ASTChecker::visit(IntNumAST & el) {

			return std::make_unique<ASTPayload>(INT,el.location);
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(FloatingNumAST & el) {
			return std::make_unique<ASTPayload>(REAL,el.location);
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(AssignmentStatementAST & el) {
			auto LHS = el.lvalue->accept(*this);
			auto RHS = el.expr->accept(*this);

			if (LHS->get_basic_type() < RHS->get_basic_type()) {
				AST_ERROR("Cannot convert from 'real' to 'int' type : {0}", RHS->loc);
			}

			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(ReturnStatementAST & el) {
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(PrintStatementAST & el) {
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(ReadStatementAST & el) {
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(IfStatementAST & el) {
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

		std::unique_ptr<ASTPayload> ASTChecker::visit(ForStatementAST & el) {
			bool has_outer_block = this->is_inner_stmt_block ? true : false;
			this->is_inner_stmt_block = !has_outer_block;
			el.assign_statement->accept(*this);
			el.to_expr->accept(*this);
			if (el.by_expr)
				el.by_expr->accept(*this);
			el.statement_block->accept(*this);
			this->is_inner_stmt_block = has_outer_block;
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(WhileStatementAST & el) {
			bool has_outer_block = this->is_inner_stmt_block ? true : false;
			this->is_inner_stmt_block = !has_outer_block;
			el.while_expr->accept(*this);
			el.statement_block->accept(*this);
			this->is_inner_stmt_block = has_outer_block;
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(InvocationAST & el) {
			if (!func_tab.count(el.callee)) {
				AST_ERROR("Unknown func name \"{0}\" : {1}", el.callee, el.location);
			} else {
				for (auto& arg : el.args) {
					arg->accept(*this);
				}
			}
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(StatementAST & el) {
			return nullptr;
		}

		std::unique_ptr<ASTPayload> ASTChecker::visit(StatementBlockAST & el) {
			auto has_ret = false;
			auto it = el.statement_list.begin();
			while (it != el.statement_list.end()) {
				if (has_ret || ret_in_statement) {
					VEX_INFO("Removing rest of the statements in {0}", this->func_name);
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
			if (!has_ret && !ret_in_statement && !is_inner_stmt_block) {
				// Statement block has no return statement, we add it instead
				VEX_INFO("'return' not found, placing 1 at the end of func {0}", this->func_name);
				if (this->func_type == INT) {
					el.statement_list.emplace_back(
						std::make_unique<ReturnStatementAST>(
							std::make_unique<IntNumAST>(1)
							));
				} else if (this->func_type == REAL) {
					el.statement_list.emplace_back(
						std::make_unique<ReturnStatementAST>(
							std::make_unique<FloatingNumAST>(1.0)
							));
				}
			} else {
				this->ret_in_statement = false;

			}
			return nullptr;
		}

		bool ASTChecker::get_err() {
			return err;
		}

		Type* ASTChecker::find_sym(const std::string & var_name) {
			if (auto res = sym_tab[var_name]) {
				return res;
			} else {
				return global_tab[var_name];
			}
			return nullptr;
		}
	}
