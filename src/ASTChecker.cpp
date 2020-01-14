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
		VEX_ASSERT(func_tab["main"], "Function \"main\" doesn't exist!");
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
		if (func_tab[el.name]) {
			AST_ERROR("Redeclaration of function \"{0}\" : {1}", el.name, el.location);
		} else {
			std::vector<Type*> param_vec;
			// To save some allocations
			param_vec.reserve(4);
			for (const auto& param : el.parameter_list) {
				param_vec.push_back(param->var_type.get());
			}
			func_tab[el.name] = std::make_unique<FuncPayload>(
				std::move(param_vec), el.func_type, el.name, el.location);
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

		return std::move(*LHS < *RHS ? RHS : LHS);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(UnaryExprAST& el) {
		return el.LHS->accept(*this);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(VariableAST& el) {
		std::unique_ptr<ASTPayload> ret_val = nullptr;
		Type* var_type = nullptr;
		if ((var_type = find_sym(el.name))) {
			ret_val = std::make_unique<ASTPayload>(var_type, el.location);
		} else {
			AST_ERROR("Unknown variable name \"{0}\" : {1}", el.name, el.location);
			return nullptr;
		}

		if (el.indexExpr) {
			auto idx_ty = el.indexExpr->accept(*this);
			AST_ASSERT(idx_ty->get_basic_type() == INT,
				"Index cannot be other than unsigned integer for \"{0}\": {1}",
				el.name, el.location);
			if (auto int_expr = dynamic_cast<IntNumAST*>(el.indexExpr.get())) {
				if (int_expr->val < 1) {
					AST_ERROR("Index expr is <1 : {0}", el.location);
				} else if (*var_type->array_size && int_expr->val > * var_type->array_size) {
					AST_ERROR("Array index out of range : {0}", el.location);
				}
			}
		}
		return ret_val;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(IntNumAST& el) {
		return std::make_unique<ASTPayload>(INT, el.location);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(FloatingNumAST& el) {
		return std::make_unique<ASTPayload>(REAL, el.location);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(AssignmentStatementAST& el) {
		auto LHS = el.lvalue->accept(*this);
		auto RHS = el.expr->accept(*this);
		if (!LHS || !RHS) {
			return nullptr;
		}
		if (LHS->get_basic_type() < RHS->get_basic_type()) {
			AST_ERROR("Cannot narrow types: {0}", LHS->loc);
		} else if (RHS->get_basic_type() == VOID_TY) {
			AST_ERROR("Cannot assign void type : {0}", LHS->loc);
		}

		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(ReturnStatementAST& el) {
		auto ret_expr = el.expr->accept(*this);
		if (func_type == VOID_TY) {
			AST_ERROR("Void type cannot return : {0}", ret_expr->loc);
		}

		if ((ret_expr->ty_info && func_type < ret_expr->ty_info->s_type) ||
			func_type < ret_expr->s_type) {
			AST_ERROR("Return type and function type don't match or cannot narrow type : {0}",
				el.expr->location);
		}
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(PrintStatementAST& el) {
		for (const auto& expr : el.print_exprs) {
			expr->accept(*this);
		}
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(ReadStatementAST& el) {
		for (const auto& expr : el.read_exprs) {
			expr->accept(*this);
		}
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(IfStatementAST& el) {
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

	std::unique_ptr<ASTPayload> ASTChecker::visit(ForStatementAST& el) {
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

	std::unique_ptr<ASTPayload> ASTChecker::visit(WhileStatementAST& el) {
		bool has_outer_block = this->is_inner_stmt_block ? true : false;
		this->is_inner_stmt_block = !has_outer_block;
		el.while_expr->accept(*this);
		el.statement_block->accept(*this);
		this->is_inner_stmt_block = has_outer_block;
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(InvocationAST& el) {
		VEX_ASSERT(func_tab[el.callee], "Unknown func name \"{0}\" : {1}",
			el.callee, el.location);
		FuncPayload* func_info = func_tab[el.callee].get();
		auto counter = 0;
		for (auto& arg : el.args) {
			auto arg_payload = arg->accept(*this);
			check_compatibility(arg_payload.get(), func_info, counter);
			counter++;
		}
		AST_ASSERT(counter == func_info->params.size(),
			"Invalid signature for func {0} ( {1} vs {2} ) : {3}",
			el.callee, counter, func_info->params.size(), el.location);
		return std::make_unique<ASTPayload>(func_info->func_type, el.location);
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(StatementAST& el) {
		return nullptr;
	}

	std::unique_ptr<ASTPayload> ASTChecker::visit(StatementBlockAST& el) {
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

	std::unique_ptr<ASTPayload> ASTChecker::visit(StringLiteralAST& el) {
		return std::unique_ptr<ASTPayload>();
	}

	bool ASTChecker::get_err() {
		return err;
	}

	Type* ASTChecker::find_sym(const std::string& var_name) {
		if (auto res = sym_tab[var_name]) {
			return res;
		} else {
			return global_tab[var_name];
		}
		return nullptr;
	}
	void ASTChecker::check_compatibility(ASTPayload* arg_ty, FuncPayload* params_ty, const int& idx) {
		AST_ASSERT(idx < params_ty->params.size(),
			"Invalid parameter for func {0} : {1]", params_ty->name, params_ty->loc);
		if (auto var_ty = arg_ty->ty_info) {
			auto param_val = params_ty->params[idx];
			AST_ASSERT(param_val->s_type >= var_ty->s_type,
				"Cannot cast between types in arg {0} for call at {1} : {2}",
				idx, params_ty->name, params_ty->loc);
			AST_ASSERT(!(var_ty->is_array ^ param_val->is_array),
				"Incompatible types in arg {0} for call at {1} : {2}"
				, idx, params_ty->name, params_ty->loc);
			if (var_ty->is_array && *param_val->array_size > 0) {
				AST_ASSERT(*var_ty->array_size == *param_val->array_size,
					"Incompatible array sizes {0} and {1} for call at {2} : {3}",
					*var_ty->array_size, *param_val->array_size, params_ty->name, params_ty->loc);
			}

		}

	}
}
