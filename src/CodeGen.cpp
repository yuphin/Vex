#include "CodeGen.h"

void CodeGenVisitor::print_IR() {
	//curr_module->print(llvm::errs(), nullptr);
	//curr_module->dump();
	// curr_module->print(llvm::errs(),nullptr);
	curr_module->dump();
	/*
	std::string ir_str;
	llvm::raw_string_ostream o(ir_str);
	o << *curr_module;
	std::cout << ir_str;
	*/
}

llvm::Value* CodeGenVisitor::symbol_lookup(const std::string& name) {
	llvm::Value* v = nullptr;
	if (!unit_context->call_stack.empty()) {
		v = unit_context->call_stack.top().sym_tab[name];
		if (!v) {
			v = unit_context->sym_tab[name];
		}
	} else {
		v = unit_context->sym_tab[name];
	}

	return v;
}

llvm::Type* CodeGenVisitor::lookup_type(const Type& type) {
	if (!type.is_array) {
		if (type.s_type == INT) {
			return llvm::Type::getInt32Ty(context);
		} else if (type.s_type == REAL) {
			return llvm::Type::getDoubleTy(context);
		} else {
			return nullptr;
		}

	} else {
		llvm::ElementCount ec(4, true);
		if (type.s_type == INT && *type.array_size > 0) {
			return llvm::VectorType::get(llvm::Type::getInt32Ty(context), *type.array_size);
		} else if (type.s_type == INT) {
			return llvm::VectorType::get(llvm::Type::getInt32Ty(context), ec);


		} else if (type.s_type == REAL && *type.array_size > 0) {
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context), *type.array_size);

		} else if (type.s_type == REAL) {
			llvm::ElementCount ec(2, false);
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context), *type.array_size);
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context), ec);

		} else {
			return nullptr;
		}

	}

}

llvm::Type* CodeGenVisitor::lookup_type(int type) {
	switch (type) {
	case obj_type::INT: {
		return llvm::Type::getInt32Ty(context);

	}
	case obj_type::REAL: {
		return llvm::Type::getDoubleTy(context);
	}
	default:
		return nullptr;
	}

}

llvm::Value* CodeGenVisitor::create_cmp(llvm::Value* LHS, const llvm::Twine& name) {
	auto L_Type = LHS->getType();
	auto zero_int = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
	auto zero_float = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
	if (L_Type->isIntegerTy()) {
		return Builder->CreateICmpNE(LHS, zero_int, name);
	} else if (L_Type->isDoubleTy()) {
		return Builder->CreateFCmpONE(LHS, zero_float, name);
	}
	return nullptr;
}

llvm::AllocaInst* CodeGenVisitor::insert_alloca(llvm::Function* func, 
	const std::string& var_name,llvm::Type* type) {

	 llvm::IRBuilder<> tmp_builder(&func->getEntryBlock(),func->getEntryBlock().begin());
	 return tmp_builder.CreateAlloca(type,0,var_name);
}

llvm::Value* CodeGenVisitor::visit(IntNumAST& el) {
	return llvm::ConstantInt::get(context, llvm::APInt(32, el.val, true));
}

llvm::Value* CodeGenVisitor::visit(FloatingNumAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(el.val));
}

llvm::Value* CodeGenVisitor::visit(AssignmentStatementAST& el) {
	// TODO: proper error checks later

	llvm::Value* rhs_expr = el.expr->accept(*this);
	if (!rhs_expr) {
		return nullptr;
	}
	auto var = symbol_lookup(el.lvalue->name);
	if (!var) {
		std::cerr << "Unknown variable \n";
		return nullptr;
	}
	Builder->CreateStore(rhs_expr, var);
	// We don't really need to return var
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ReturnStatementAST& el) {
	auto ret_val = el.expr->accept(*this);
	Builder->CreateRet(ret_val);
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(IfStatementAST& el) {
	auto condition = el.if_expr->accept(*this);
	llvm::BasicBlock* else_b;
	if (!condition) {
		return nullptr;
	}
	auto has_else = (el.else_blk != nullptr);

	condition = create_cmp(condition, "if_expr");
	llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* then_b = llvm::BasicBlock::Create(context, "then", enclosing_func);
	llvm::BasicBlock* if_cont = llvm::BasicBlock::Create(context, "ifcont");
	else_b = llvm::BasicBlock::Create(context, "else");

	Builder->CreateCondBr(condition, then_b, else_b);
	Builder->SetInsertPoint(then_b);
	el.then_blk->accept(*this);
	Builder->CreateBr(if_cont);
	// Update then block since it might change the block contents

	enclosing_func->getBasicBlockList().push_back(else_b);
	Builder->SetInsertPoint(else_b);
	if (has_else) {
		el.else_blk->accept(*this);
	}
	Builder->CreateBr(if_cont);

	// if_cont part
	enclosing_func->getBasicBlockList().push_back(if_cont);
	Builder->SetInsertPoint(if_cont);

	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ForStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(WhileStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(BaseAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(TopAST& el) {
	for (auto& decl : el.declaration_list) {
		decl->accept(*this);
	}
	unit_context->in_global_namespace = false;
	for (auto& func : el.function_list) {
		// Enter function context
		unit_context->call_stack.emplace();
		func->accept(*this);
		// Exit function context
		unit_context->call_stack.pop();
	}

	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(VariableDeclAST& el) {
	if (unit_context->in_global_namespace) {
		llvm::GlobalVariable* global_var = new llvm::GlobalVariable(*curr_module,
			lookup_type(*el.var_type), false, llvm::Function::ExternalLinkage, 0, el.name, 0,
			llvm::GlobalValue::NotThreadLocal, 0, false);
		unit_context->sym_tab[el.name] = global_var;
	} else {
		// We are in the function context
		auto enclosing_func = Builder->GetInsertBlock()->getParent();
		auto val = insert_alloca(enclosing_func, el.name, 
			lookup_type(*el.var_type));
		unit_context->call_stack.top().sym_tab[el.name] = val;
	}
	return nullptr;


}

llvm::Value* CodeGenVisitor::visit(FunctionAST& el) {
	// First, check for an existing function from a previous 'extern' declaration.
	auto func = curr_module->getFunction(el.prototype->name);

	if (!func)
		func = static_cast<llvm::Function*>(el.prototype->accept(*this));
	if (!func)
		return nullptr;

	// Create a new basic block to start insertion into.
	llvm::BasicBlock* bb = llvm::BasicBlock::Create(context, "entry", func);
	Builder->SetInsertPoint(bb);

	// Record the function arguments in the symbol map.
	for (auto& arg : func->args()) {
		auto v_alloca = insert_alloca(func, arg.getName(),arg.getType());
		Builder->CreateStore(&arg, v_alloca);
		unit_context->call_stack.top().sym_tab[arg.getName()] = &arg;
	}
	el.body->accept(*this);
	llvm::verifyFunction(*func);
	return func;
	// Error reading body, remove function.
	// func->eraseFromParent();
}

llvm::Value* CodeGenVisitor::visit(FunctionDeclAST& el) {
	std::vector<llvm::Type* > args_v;
	args_v.reserve(el.parameter_list.size());
	for (auto& param : el.parameter_list) {
		args_v.push_back(lookup_type(*param->var_type));
	}
	// auto lt = lookup_type(static_cast<std::underlying_type_t<int>>(el.func_type));
	llvm::FunctionType* ft = llvm::FunctionType::get(lookup_type(static_cast<int>(el.func_type)), args_v, false);

	llvm::Function* f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
		el.name, curr_module.get());
	unsigned int idx = 0;
	for (auto& arg : f->args()) {
		arg.setName(el.parameter_list[idx++]->name);
	}
	return f;
}

llvm::Value* CodeGenVisitor::visit(FunctionBodyAST& el) {
	for (auto& decl : el.declaration_list) {
		decl->accept(*this);
	}
	for (auto& stat : el.statement_block->statement_list) {
		stat->accept(*this);
	}
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(BinaryExprAST& el) {
	llvm::Value* L = el.LHS->accept(*this);
	llvm::Value* R = el.RHS->accept(*this);
	auto L_type = L->getType();
	auto R_type = R->getType();
	if (!L || !R)
		return nullptr;



	switch (el.binop) {
	case ADD: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isIntegerTy(32)) {
				return Builder->CreateAdd(L, R, "addtmp");
			} else if (L_type->isDoubleTy()) {
				return Builder->CreateFAdd(L, R, "addtmp");
			} else {
				/* New types might be added in the future */
			}

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFAdd(L, R, "addtmp");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFAdd(L, R, "addtmp");
		}
	}
	case SUB: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isIntegerTy(32)) {
				return Builder->CreateSub(L, R, "subtmp");
			} else if (L_type->isDoubleTy()) {
				return Builder->CreateFSub(L, R, "subtmp");
			}
		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFSub(L, R, "subtmp");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFSub(L, R, "subtmp");
		}
	}
	case MULT: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isIntegerTy(32)) {
				return Builder->CreateMul(L, R, "tmpmult");
			} else if (L_type->isDoubleTy()) {
				return Builder->CreateFMul(L, R, "tmpmult");
			} else {
				/* New types might be added in the future */
			}

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFMul(L, R, "tmpmult");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFMul(L, R, "tmpmult");
		}
	}
	case LT: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateFCmpULT(L, R, "tmplt");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFCmpULT(L, R, "tmplt");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFCmpULT(L, R, "tmplt");
		}

	}
	case GT: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateFCmpUGT(L, R, "tmpgt");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFCmpUGT(L, R, "tmpgt");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFCmpUGT(L, R, "tmpgt");
		}
	}
	case LTE: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateFCmpULE(L, R, "tmplte");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFCmpULE(L, R, "tmplte");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFCmpULE(L, R, "tmplte");
		}
	}
	case GTE: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateFCmpUGE(L, R, "tmpgte");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFCmpUGE(L, R, "tmpgte");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFCmpUGE(L, R, "tmpgte");
		}
	}
	case EQ: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateFCmpUEQ(L, R, "tmpeq");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFCmpUEQ(L, R, "tmpeq");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFCmpUEQ(L, R, "tmpeq");
		}
	}
	case AND: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateAnd(L, R, "tmpand");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateAnd(L, R, "tmpand");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateAnd(L, R, "tmpand");
		}
	}
	case OR: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			return Builder->CreateOr(L, R, "tmpor");

		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateOr(L, R, "tmpor");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateOr(L, R, "tmpor");
		}
	}
	case MOD: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isIntegerTy(32)) {
				return Builder->CreateSRem(L, R, "tmpmod");
			} else if (L_type->isDoubleTy()) {
				return Builder->CreateFRem(L, R, "tmpmod");
			} else {
				/* New types might be added in the future */
			}
		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFRem(L, R, "tmpmod");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFRem(L, R, "tmpmod");
		}	}
	case DIV: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isDoubleTy()) {
				return Builder->CreateFDiv(L, R, "tmpdiv");
			}
		} else if (L_type->isDoubleTy() && !R_type->isDoubleTy()) {
			Builder->CreateSIToFP(R, L_type, "casttmp");
			return Builder->CreateFDiv(L, R, "tmpdiv");

		} else if (R_type->isDoubleTy() && !L_type->isDoubleTy()) {
			Builder->CreateSIToFP(L, R_type, "casttmp");
			return Builder->CreateFDiv(L, R, "tmpdiv");
		}
	}
	case IDIV: {
		if (typeid(L_type).hash_code() == typeid(R_type).hash_code()) {
			if (L_type->isIntegerTy()) {
				return Builder->CreateFDiv(L, R, "tmpidiv");
			}
		}
	}
	default: {
		std::cerr << "Invalid binary operator\n";
		return nullptr;
	}
	}

	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(UnaryExprAST& el) {
	llvm::Value* V = el.LHS->accept(*this);
	switch (el.unop) {
	case UNOT: {
		return Builder->CreateNot(V, "tmpnot");
	}
	case MINUS: {
		auto V_type = V->getType();
		if (V_type->isDoubleTy()) {
			return Builder->CreateNeg(V, "tmpneg");
		} else if (V_type->isIntegerTy()) {
			return Builder->CreateFNeg(V, "tmpneg");
		}
	}
	default: {
		std::cerr << "Invalid binary operator\n";
		return nullptr;
	}
	}
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(VariableAST& el) {
	llvm::Value* v = symbol_lookup(el.name);
	if (!v) {
		// To be replaced with logger
		return nullptr;
		std::cerr << "Unknown variable!\n";
	}
	return Builder->CreateLoad(v, el.name);;

}

llvm::Value* CodeGenVisitor::visit(InvocationAST& el) {
	llvm::Function* callee = curr_module->getFunction(el.callee);

	if (!callee) {
		std::cerr << "Unknown function\n";
		return nullptr;
	}
	if (callee->arg_size() != el.args.size()) {
		std::cerr << "Incorrect arguments\n";
		return nullptr;
	}
	std::vector<llvm::Value*> args_vector;
	for (unsigned i = 0, e = el.args.size(); i != e; ++i) {
		args_vector.push_back(el.args[i]->accept(*this));
		if (!args_vector.back())
			return nullptr;
	}
	return Builder->CreateCall(callee, args_vector, "callfunc");

}

llvm::Value* CodeGenVisitor::visit(StatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(StatementBlockAST& el) {
	return nullptr;
}
