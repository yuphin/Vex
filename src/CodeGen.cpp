#include "CodeGen.h"

void CodeGen::print_IR() {
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

llvm::Value* CodeGen::symbol_lookup(const std::string& name) {
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

llvm::Type* CodeGen::lookup_type(const Type& type) {
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

llvm::Type* CodeGen::lookup_type(int type) {
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

llvm::Type* CodeGen::get_type(llvm::Value* V, bool underlying_type = false) {
	llvm::Type* t;
	if (auto ai = llvm::dyn_cast<llvm::AllocaInst>(V)) {
		t = ai->getAllocatedType();
	} else if (auto ai = llvm::dyn_cast<llvm::GlobalValue>(V)) {
		t = ai->getValueType();
	} else {
		t = V->getType();
	}

	if (underlying_type) {
		if (auto ai = llvm::dyn_cast<llvm::SequentialType>(t)) {
			t = ai->getElementType();
		}
	}
	return t;
}


llvm::Value* CodeGen::create_binary(llvm::Value* LHS, llvm::Value* RHS, int op, const llvm::Twine& name = "") {
	llvm::Type* l_type = get_type(LHS, true);

	switch (op) {
	case EQ: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_EQ, LHS, RHS, "tmpeq");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_OEQ, LHS, RHS, "tmpeq");
		}
		break;
	}
	case NEQ: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_NE, LHS, RHS, "tmpneq");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_ONE, LHS, RHS, "tmpneq");
		}
		break;
	}
	case LT: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_SLT, LHS, RHS, "tmplt");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_OLT, LHS, RHS, "tmplt");
		}
		break;
	}
	case GT: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_SGT, LHS, RHS, "tmpgt");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_OGT, LHS, RHS, "tmpgt");
		}
		break;
	}
	case LTE: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_SLE, LHS, RHS, "tmplte");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_OLE, LHS, RHS, "tmplte");
		}
		break;
	}
	case GTE: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateICmp(llvm::CmpInst::ICMP_SGE, LHS, RHS, "tmpgte");
		} else {
			return Builder->CreateFCmp(llvm::CmpInst::FCMP_OGE, LHS, RHS, "tmpgte");
		}
		break;
	}
	case AND: {
		// Logical and
		auto r_type = get_type(RHS, true);
		auto prev_insert = Builder->GetInsertBlock();
		llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
		llvm::BasicBlock* first_block = llvm::BasicBlock::Create(context, "and_1", enclosing_func);
		llvm::BasicBlock* second_block = llvm::BasicBlock::Create(context, "and_exit", enclosing_func);

		llvm::Value* zero_val_l;
		llvm::Value* zero_val_r;
		if (l_type->isIntegerTy()) {
			zero_val_l = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
		} else {
			zero_val_l = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
		}

		if (r_type->isIntegerTy()) {
			zero_val_r = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
		} else {
			zero_val_r = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
		}

		// Check if the first expr is not equal to 0 
		auto not_zero_l = create_binary(LHS, zero_val_l, NEQ);

		Builder->CreateCondBr(not_zero_l, first_block, second_block);
		Builder->SetInsertPoint(first_block);
		auto not_zero_r = create_binary(RHS, zero_val_r, NEQ);
		Builder->CreateBr(second_block);
		Builder->SetInsertPoint(second_block);
		llvm::PHINode* pn = Builder->CreatePHI(llvm::Type::getInt1Ty(context), 2, "and_merge");
		auto false_t = llvm::ConstantInt::getFalse(llvm::Type::getInt1Ty(context));
		pn->addIncoming(false_t, prev_insert);
		pn->addIncoming(not_zero_r, first_block);
		return pn;

		break;
	}
	case OR: {
		// Logical or
		auto r_type = get_type(RHS, true);
		auto prev_insert = Builder->GetInsertBlock();
		llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
		llvm::BasicBlock* first_block = llvm::BasicBlock::Create(context, "or_1", enclosing_func);
		llvm::BasicBlock* second_block = llvm::BasicBlock::Create(context, "or_exit", enclosing_func);

		llvm::Value* zero_val_l;
		llvm::Value* zero_val_r;
		if (l_type->isIntegerTy()) {
			zero_val_l = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
		} else {
			zero_val_l = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
		}

		if (r_type->isIntegerTy()) {
			zero_val_r = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
		} else {
			zero_val_r = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
		}

		// Check if the first expr is not equal to 0 
		auto zero_l = create_binary(LHS, zero_val_l, EQ);

		Builder->CreateCondBr(zero_l, first_block, second_block);
		Builder->SetInsertPoint(first_block);
		auto zero_r = create_binary(RHS, zero_val_r, NEQ);
		Builder->CreateBr(second_block);
		Builder->SetInsertPoint(second_block);
		llvm::PHINode* pn = Builder->CreatePHI(llvm::Type::getInt1Ty(context), 2, "and_e");
		auto true_t = llvm::ConstantInt::getTrue(llvm::Type::getInt1Ty(context));
		pn->addIncoming(true_t, prev_insert);
		pn->addIncoming(zero_r, first_block);
		return pn;

		break;

	}
	case NOT: {

		break;
	}
	case ADD: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateAdd(LHS, RHS, "addtmp");
		} else {
			return Builder->CreateFAdd(LHS, RHS, "addtmp");
		}
		break;
	}
	case SUB: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateSub(LHS, RHS, "subtmp");
		} else {
			return Builder->CreateFSub(LHS, RHS, "subtmp");
		}
		break;
	}
	case MULT: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateMul(LHS, RHS, "multmp");
		} else {
			return Builder->CreateFMul(LHS, RHS, "multmp");
		}
		break;
	}
	case DIV: {
		if (l_type->isIntegerTy()) {
			// Never going to be called
			return nullptr;
		} else {
			return Builder->CreateFDiv(LHS, RHS, "tmpdiv");
		}
		break;
	}
	case IDIV: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateSDiv(LHS, RHS, "tmpidiv");
		} else {
			// Never going to be called
			return nullptr;
		}
		break;
	}
	case MOD: {
		if (l_type->isIntegerTy()) {
			return Builder->CreateSRem(LHS, RHS, "tmpmod");
		} else {
			return Builder->CreateFRem(LHS, RHS, "tmpmod");
		}
		break;
	}
	default:
		std::cerr << "Invalid binary operator\n";
		break;
	}
	return nullptr;
}

std::pair<llvm::Value*, llvm::Value*> CodeGen::cast_values(llvm::Value* LHS, llvm::Value* RHS) {
	llvm::Type* l_type = get_type(LHS, true);
	llvm::Type* r_type = get_type(RHS, true);

	if (l_type == r_type) {
		return std::make_pair(LHS, RHS);

	} else if (l_type->isIntegerTy() && r_type->isDoubleTy()) {
		auto casted_LHS = Builder->CreateSIToFP(LHS, get_type(RHS), "casttmp");
		return std::make_pair(casted_LHS, RHS);
	} else {
		auto casted_RHS = Builder->CreateSIToFP(RHS, get_type(LHS), "casttmp");
		return std::make_pair(LHS, casted_RHS);

	}
}

llvm::Value* CodeGen::create_cmp(llvm::Value* LHS, llvm::Value* RHS,
	llvm::CmpInst::Predicate P, const llvm::Twine& name) {
	llvm::Type* l_type;
	llvm::Type* r_type;
	if (auto ail = llvm::dyn_cast<llvm::AllocaInst>(LHS)) {
		l_type = ail->getAllocatedType();
	} else {
		l_type = LHS->getType();
	}
	if (auto air = llvm::dyn_cast<llvm::AllocaInst>(RHS)) {
		r_type = air->getAllocatedType();
	} else {
		r_type = RHS->getType();
	}

	if (l_type->isDoubleTy() || r_type->isDoubleTy()) {
		return Builder->CreateFCmp(P, LHS, RHS, name);
	} else {
		return Builder->CreateICmp(P, LHS, RHS, name);
	}
	return nullptr;

}


llvm::AllocaInst* CodeGen::insert_alloca_to_top(llvm::Function* func,
	const std::string& var_name, llvm::Type* type) {

	llvm::IRBuilder<> tmp_builder(&func->getEntryBlock(), func->getEntryBlock().begin());
	return tmp_builder.CreateAlloca(type, 0, var_name);
}

llvm::Value* CodeGen::cast_according_to(llvm::Value* LHS, llvm::Value* RHS) {
	llvm::Type* l_type = get_type(LHS, true);
	llvm::Type* r_type = get_type(RHS, true);

	if (l_type == r_type) {
		return RHS;

	} else if (l_type->isIntegerTy() && r_type->isDoubleTy()) {
		auto casted_RHS = Builder->CreateFPToSI(RHS, get_type(LHS), "casttmp");
		return casted_RHS;
	} else {
		auto casted_RHS = Builder->CreateSIToFP(RHS, get_type(LHS), "casttmp");
		return casted_RHS;

	}

	return nullptr;
}

llvm::Value* CodeGen::cast_according_to_t(llvm::Type* l_type, llvm::Value* RHS) {
	llvm::Type* r_type = get_type(RHS, true);

	if (l_type == r_type) {
		return RHS;

	} else if (l_type->isIntegerTy() && r_type->isDoubleTy()) {
		auto casted_RHS = Builder->CreateFPToSI(RHS, l_type, "casttmp");
		return casted_RHS;
	} else {
		auto casted_RHS = Builder->CreateSIToFP(RHS, l_type, "casttmp");
		return casted_RHS;

	}

	return nullptr;
}

llvm::Value* CodeGen::visit(IntNumAST& el) {
	return llvm::ConstantInt::get(context, llvm::APInt(32, el.val, true));
}

llvm::Value* CodeGen::visit(FloatingNumAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(el.val));
}

llvm::Value* CodeGen::visit(AssignmentStatementAST& el) {
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
	auto casted = cast_according_to(var, rhs_expr);
	Builder->CreateStore(casted, var);
	// We don't really need to return var
	return casted;
}

llvm::Value* CodeGen::visit(ReturnStatementAST& el) {
	auto& return_val = unit_context->call_stack.top().return_val;
	auto& return_br = unit_context->call_stack.top().return_br;
	auto enclosing_func = Builder->GetInsertBlock()->getParent();
	if (unit_context->in_statement) {
		auto ret_expr = el.expr->accept(*this);
		if (!return_val) {
			return_br = llvm::BasicBlock::Create(context, "return_label");
			return_val = insert_alloca_to_top(
				enclosing_func,
				"retval",
				enclosing_func->getReturnType()
			);
		}
		if (ret_expr->getType() != enclosing_func->getReturnType()) {
			ret_expr = cast_according_to_t(enclosing_func->getReturnType(), ret_expr);
		}
		Builder->CreateStore(ret_expr, return_val);
		Builder->CreateBr(return_br);
		return return_br;

	} else if (!return_val) {
		auto ret_val = el.expr->accept(*this);
		Builder->CreateRet(ret_val);
	} else {
		enclosing_func->getBasicBlockList().push_back(return_br);
		Builder->SetInsertPoint(return_br);
		auto val = Builder->CreateLoad(enclosing_func->getReturnType(), return_val, "retval");
		Builder->CreateRet(val);

	}
	return nullptr;
}

llvm::Value* CodeGen::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(IfStatementAST& el) {
	unit_context->in_statement = true;
	auto condition = el.if_expr->accept(*this);
	llvm::BasicBlock* else_b = nullptr;
	if (!condition) {
		return nullptr;
	}
	auto has_else = (el.else_blk != nullptr);
	auto condition_type = get_type(condition);
	llvm::Value* zero_val;
	if (condition_type->isIntegerTy() && condition_type->getIntegerBitWidth() == 32) {
		zero_val = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
		condition = create_binary(condition, zero_val, NEQ);
	} else if (condition_type->isDoubleTy()) {
		zero_val = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
		condition = create_binary(condition, zero_val, NEQ);
	}
	llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* then_b = llvm::BasicBlock::Create(context, "then", enclosing_func);
	llvm::BasicBlock* if_cont = llvm::BasicBlock::Create(context, "if_cont");
	if (has_else) {
		else_b = llvm::BasicBlock::Create(context, "else");
		Builder->CreateCondBr(condition, then_b, else_b);
	} else {
		Builder->CreateCondBr(condition, then_b, if_cont);
	}
	Builder->SetInsertPoint(then_b);
	auto return_label = el.then_blk->accept(*this);
	if (!return_label) {
		Builder->CreateBr(if_cont);
	}


	if (has_else) {
		enclosing_func->getBasicBlockList().push_back(else_b);
		Builder->SetInsertPoint(else_b);
		return_label = el.else_blk->accept(*this);
		if (!return_label) {
			Builder->CreateBr(if_cont);
		}
	}

	// if_cont part
	//auto lel = llvm::predecessors(if_cont).begin(;
	if (llvm::predecessors(if_cont).begin() != llvm::predecessors(if_cont).end()) {
		enclosing_func->getBasicBlockList().push_back(if_cont);
		Builder->SetInsertPoint(if_cont);
	}

	unit_context->in_statement = false;
	return nullptr;

}

llvm::Value* CodeGen::visit(ForStatementAST& el) {
	unit_context->in_statement = true;
	llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
	auto assigned_value = el.assign_statement->accept(*this);
	auto test_block = llvm::BasicBlock::Create(context, "looptest", enclosing_func);
	auto loop_block = llvm::BasicBlock::Create(context, "loop", enclosing_func);
	auto step_block = llvm::BasicBlock::Create(context, "loopstep", enclosing_func);
	auto cont_block = llvm::BasicBlock::Create(context, "loopcont",enclosing_func);
	// Jump to loop block for testing
	Builder->CreateBr(test_block);
	Builder->SetInsertPoint(test_block);
	auto to_expr = el.to_expr->accept(*this);

	// Returns pair of potentially casted values
	auto casted = cast_values(assigned_value, to_expr);
	auto cmp_expr = create_binary(casted.first, casted.second, NEQ, "to_comp");
	// Do the test, end for if necessary
	Builder->CreateCondBr(cmp_expr, loop_block, cont_block);

	// enclosing_func->getBasicBlockList().push_back(loop_block);
	Builder->SetInsertPoint(loop_block);
	auto return_label = el.statement_block->accept(*this);
	// Go to the step part

	Builder->SetInsertPoint(step_block);
	if (!return_label) {
		Builder->CreateBr(step_block);

	}


	// Potentially cast Lvalue or ByExpr to double 
	if (el.by_expr) {
		casted = cast_values(assigned_value, el.by_expr->accept(*this));
	} else if (assigned_value->getType()->isIntegerTy()) {
		auto one_int = llvm::ConstantInt::get(context, llvm::APInt(32, 1, true));
		casted = std::make_pair(assigned_value, one_int);
	} else {
		auto one_float = llvm::ConstantFP::get(context, llvm::APFloat(1.0));
		casted = std::make_pair(assigned_value, one_float);
	}
	// Example : Lvalue-> Int, ByExpr-> Double then we want to do double+double
	auto add_val = create_binary(casted.first, casted.second, ADD);
	// We need to load Lvalue
	auto l_value = symbol_lookup(el.assign_statement->lvalue->name);
	auto l_type = get_type(l_value);
	Builder->CreateLoad(l_type, l_value, l_value->getName());
	// Before storing to Lvalue, we need to convert back to Lvalue's type regardless
	auto casted_add = cast_according_to(l_value, add_val);
	Builder->CreateStore(casted_add, l_value);
	Builder->CreateBr(test_block);
	// End of the loop
	Builder->SetInsertPoint(cont_block);

	unit_context->in_statement = false;
	return nullptr;
}

llvm::Value* CodeGen::visit(WhileStatementAST& el) {
	unit_context->in_statement = true;
	llvm::Function* enclosing_func = Builder->GetInsertBlock()->getParent();
	auto test_block = llvm::BasicBlock::Create(context, "looptest", enclosing_func);
	auto loop_block = llvm::BasicBlock::Create(context, "loop", enclosing_func);
	auto cont_block = llvm::BasicBlock::Create(context, "loopcont", enclosing_func);
	// Jump to loop block for testing
	Builder->CreateBr(test_block);
	auto while_val = el.while_expr->accept(*this);

	llvm::Value* zero_val;
	auto expr_type = get_type(while_val);
	if (expr_type->isIntegerTy()) {
		zero_val = llvm::ConstantInt::getFalse(llvm::Type::getInt1Ty(context));
	} else {
		zero_val = llvm::ConstantFP::get(context, llvm::APFloat(0.0));
	}
	while_val = create_binary(while_val, zero_val, NEQ, "while_expr");
	// Do the test, end while if necessary
	Builder->CreateCondBr(while_val, loop_block, cont_block);

	// Loop block
	Builder->SetInsertPoint(loop_block);
	auto return_label = el.statement_block->accept(*this);
	if (!return_label) {
		// Go to the beginning of the loop for testing
		Builder->CreateBr(test_block);
	}
	// After loop
	Builder->SetInsertPoint(cont_block);
	unit_context->in_statement = false;
	return nullptr;
}

llvm::Value* CodeGen::visit(BaseAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(TopAST& el) {
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


	mpm.run(*curr_module);


	return nullptr;
}

llvm::Value* CodeGen::visit(VariableDeclAST& el) {
	if (unit_context->in_global_namespace) {
		llvm::GlobalVariable* global_var = new llvm::GlobalVariable(*curr_module,
			lookup_type(*el.var_type), false, llvm::Function::ExternalLinkage, 0, el.name, 0,
			llvm::GlobalValue::NotThreadLocal, 0, false);
		unit_context->sym_tab[el.name] = global_var;
	} else {
		// We are in the function context
		auto enclosing_func = Builder->GetInsertBlock()->getParent();
		auto val = insert_alloca_to_top(enclosing_func, el.name,
			lookup_type(*el.var_type));
		unit_context->call_stack.top().sym_tab[el.name] = val;
	}
	return nullptr;


}

llvm::Value* CodeGen::visit(FunctionAST& el) {
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
		auto v_alloca = insert_alloca_to_top(func, arg.getName(), arg.getType());
		Builder->CreateStore(&arg, v_alloca);
		unit_context->call_stack.top().sym_tab[arg.getName()] = v_alloca;
	}
	el.body->accept(*this);
	llvm::verifyFunction(*func);
	fpm->run(*func);
	return func;
	// Error reading body, remove function.
	// func->eraseFromParent();
}

llvm::Value* CodeGen::visit(FunctionDeclAST& el) {
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

llvm::Value* CodeGen::visit(FunctionBodyAST& el) {
	for (auto& decl : el.declaration_list) {
		decl->accept(*this);
	}
	el.statement_block->accept(*this);
	return nullptr;
}

llvm::Value* CodeGen::visit(ExprAST& el) {
	return nullptr;
}

llvm::Value* CodeGen::visit(BinaryExprAST& el) {
	llvm::Value* L = el.LHS->accept(*this);
	llvm::Value* R = el.RHS->accept(*this);
	if (el.binop != AND || el.binop != OR) {
		auto casted = cast_values(L, R);
		return create_binary(casted.first, casted.second, el.binop);
	} else {
		return create_binary(L, R, el.binop);
	}
}

llvm::Value* CodeGen::visit(UnaryExprAST& el) {
	llvm::Value* V = el.LHS->accept(*this);
	auto V_type = get_type(V);
	switch (el.unop) {
	case UNOT: {
		return Builder->CreateNot(V, "tmpnot");
	}
	case MINUS: {
		if (V_type->isIntegerTy()) {
			return Builder->CreateNeg(V, "tmpneg");
		} else if (V_type->isDoubleTy()) {
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

llvm::Value* CodeGen::visit(VariableAST& el) {
	llvm::Value* v = symbol_lookup(el.name);
	if (!v) {
		// To be replaced with logger
		return nullptr;
		std::cerr << "Unknown variable!\n";
	}
	auto type = get_type(v);
	return Builder->CreateLoad(type, v, el.name);;

}

llvm::Value* CodeGen::visit(InvocationAST& el) {
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

llvm::Value* CodeGen::visit(StatementAST& el) {

	return nullptr;
}

llvm::Value* CodeGen::visit(StatementBlockAST& el) {
	llvm::Value* val = nullptr;
	for (auto& stat : el.statement_list) {
		val = stat->accept(*this);
	}

	return val;
}
