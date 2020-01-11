#include "ASTPrinter.h"

namespace Vex {

	void ASTPrinter::visit(BaseAST& el) {}

	void ASTPrinter::visit(TopAST& el) {
		Logger::set_printer_mode();
		for (const auto& decl : el.declaration_list) {
			decl->accept(*this);
		}

		for (const auto& func : el.function_list) {
			func->accept(*this);
		}
		Logger::set_default_mode();
	}

	void ASTPrinter::visit(VariableDeclAST& el) {
		VEX_TRACE("{0}-Var decl: {1} : {2}",
			get_alignment(indent_counter), el.name, print_var_type(el.var_type.get()));
	}

	void ASTPrinter::visit(FunctionAST& el) {
		el.prototype->accept(*this);
		indent_counter++;
		el.body->accept(*this);
		indent_counter--;
	}


	void ASTPrinter::visit(FunctionDeclAST& el) {
		VEX_TRACE("{0}-Func decl: {1} : {2}",
			get_alignment(indent_counter), el.name, print_var_type(el.func_type));
	}

	void ASTPrinter::visit(FunctionBodyAST& el) {
		for (const auto& decl : el.declaration_list) {
			decl->accept(*this);
		}
		el.statement_block->accept(*this);
	}

	void ASTPrinter::visit(ExprAST& el) {}

	void ASTPrinter::visit(BinaryExprAST& el) {
		VEX_TRACE("{0}-Binary expr:", get_alignment(indent_counter));
		indent_counter++;
		el.LHS->accept(*this);
		el.RHS->accept(*this);
		indent_counter--;
	}

	void ASTPrinter::visit(UnaryExprAST& el) {
		// Currently only '-' 
		VEX_TRACE("{0}-Unary operator {1]:",
			get_alignment(indent_counter), print_var_type(el.unop));
		indent_counter++;
		el.LHS->accept(*this);
		indent_counter--;
	}

	void ASTPrinter::visit(VariableAST& el) {
		VEX_TRACE("{0}-Var ref : {1}",
			get_alignment(indent_counter), el.name);
		indent_counter++;
		if (el.indexExpr) {
			el.indexExpr->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(IntNumAST& el) {
		VEX_TRACE("{0}-Literal : {1}",
			get_alignment(indent_counter), el.val);
	}

	void ASTPrinter::visit(FloatingNumAST& el) {
		VEX_TRACE("{0}-Literal : {1}",
			get_alignment(indent_counter), el.val);
	}

	void ASTPrinter::visit(InvocationAST& el) {
		VEX_TRACE("{0}-Func call {1}",
			get_alignment(indent_counter), el.callee);
		indent_counter++;
		for (const auto& arg : el.args) {
			arg->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(StatementBlockAST& el) {
		indent_counter++;
		for (const auto& stmt : el.statement_list) {
			stmt->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(StatementAST& el) {}

	void ASTPrinter::visit(AssignmentStatementAST& el) {
		VEX_TRACE("{0}-Assignment stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		el.lvalue->accept(*this);
		el.expr->accept(*this);
		indent_counter--;
	}

	void ASTPrinter::visit(ReturnStatementAST& el) {
		VEX_TRACE("{0}-Return stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		el.expr->accept(*this);
		indent_counter--;
	}

	void ASTPrinter::visit(PrintStatementAST& el) {
		VEX_TRACE("{0}-Print stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		for (const auto& expr : el.print_exprs) {
			expr->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(ReadStatementAST& el) {
		VEX_TRACE("{0}-Read stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		for (const auto& expr : el.read_exprs) {
			expr->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(IfStatementAST& el) {
		VEX_TRACE("{0}-If stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		el.if_expr->accept(*this);
		el.then_blk->accept(*this);
		if (el.else_blk) {
			el.else_blk->accept(*this);
		}
		indent_counter--;
	}

	void ASTPrinter::visit(ForStatementAST& el) {
		VEX_TRACE("{0}-For stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		el.assign_statement->accept(*this);
		el.to_expr->accept(*this);
		if (el.by_expr) {
			el.by_expr->accept(*this);
		}
		el.statement_block->accept(*this);
		indent_counter--;
	}

	void ASTPrinter::visit(WhileStatementAST& el) {
		VEX_TRACE("{0}-For stmt:",
			get_alignment(indent_counter));
		indent_counter++;
		el.while_expr->accept(*this);
		el.statement_block->accept(*this);
		indent_counter--;
	}

	std::string ASTPrinter::get_alignment(int indent) {

		return std::string(indent, ' ');
	}

	std::string ASTPrinter::print_var_type(Type* ty) {
		std::stringstream ss;
		ss << (ty->s_type == INT ? "int" : "real");
		if (ty->is_array && *ty->array_size > 0) {
			ss << "[" << *ty->array_size << "]";
		} else if (ty->is_array) {
			ss << "[]";
		}
		return ss.str();
	}

	std::string ASTPrinter::print_var_type(obj_type ty) {
		if (ty == INT) {
			return "int";
		}
		return "real";
	}

	std::string ASTPrinter::print_var_type(un_op op) {
		if (op == UNOT) {
			return "'not'";
		}
		return "'-'";
	}
}