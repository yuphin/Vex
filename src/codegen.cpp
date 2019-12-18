#include "CodeGen.h"

void CodeGenVisitor::print_IR() {
	curr_module->print(llvm::errs(), nullptr);
}

llvm::Value* CodeGenVisitor::symbol_lookup(const std::string& name) {
	llvm::Value* v= nullptr;
	if (!unit_context->call_stack.empty()) {
		v = unit_context->call_stack.top()[name];
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
		llvm::ElementCount ec(2, false);
		if (type.s_type == INT && *type.array_size > 0) {
			return llvm::VectorType::get(llvm::Type::getInt32Ty(context),*type.array_size);
		} else if (type.s_type == INT) {
			return llvm::VectorType::get(llvm::Type::getInt32Ty(context),ec);


		} else if (type.s_type == REAL && *type.array_size > 0) {
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context),*type.array_size);

		} else if (type.s_type == REAL) {
			llvm::ElementCount ec(2, false);
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context),*type.array_size);
			return llvm::VectorType::get(llvm::Type::getDoubleTy(context),ec);

		}else {
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
llvm::Value* CodeGenVisitor::visit(IntNumAST& el) {
	return llvm::ConstantInt::get(context, llvm::APInt(32, el.val, true));
}

llvm::Value* CodeGenVisitor::visit(FloatingNumAST& el) {
	return llvm::ConstantFP::get(context, llvm::APFloat(el.val));
}

llvm::Value* CodeGenVisitor::visit(AssignmentStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ReturnStatementAST& el) {

	return el.expr->accept(*this);
}

llvm::Value* CodeGenVisitor::visit(PrintStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(ReadStatementAST& el) {
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(IfStatementAST& el) {
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
	llvm::Value* result;
	for (auto& decl:  el.declaration_list) {
		decl->accept(*this);
	}
	for (auto& func : el.function_list) {
		unit_context->call_stack.emplace();
		result = func->accept(*this);
		if (result) {
			result->print(llvm::errs());
		}
		unit_context->call_stack.pop();
	}
	
	return result;
}

llvm::Value* CodeGenVisitor::visit(VariableDeclAST& el) {
	llvm::Value* v = symbol_lookup(el.name);
	if (!v) {
		// To be replaced with logger
		return nullptr;
		std::cerr << "Unknown variable!\n";
	}
	return v;
}

llvm::Value* CodeGenVisitor::visit(FunctionAST& el) {
	// First, check for an existing function from a previous 'extern' declaration.
    auto func = curr_module->getFunction(el.prototype->name);

	if (!func)
		func =  static_cast<llvm::Function*>(el.prototype->accept(*this));
	if (!func)
		return nullptr;

	// Create a new basic block to start insertion into.
	llvm::BasicBlock* bb = llvm::BasicBlock::Create(context, "entry",func);
	Builder->SetInsertPoint(bb);

	// Record the function arguments in the NamedValues map.
	for (auto& arg : func->args())
		unit_context->call_stack.top()[arg.getName()] = &arg;

	if (llvm::Value* ret_val = el.body->accept(*this)) {
		// Finish off the function.
		Builder->CreateRet(ret_val);

		// Validate the generated code, checking for consistency.
		llvm::verifyFunction(*func);

		return func;
	}
	// Error reading body, remove function.
	func->eraseFromParent();
	return nullptr;
}

llvm::Value* CodeGenVisitor::visit(FunctionDeclAST& el) {
	std::vector<llvm::Type* > args_v;
	args_v.reserve(el.parameter_list.size());
	for ( auto& param : el.parameter_list) {
		args_v.push_back(lookup_type(*param->var_type));
	}
	// auto lt = lookup_type(static_cast<std::underlying_type_t<int>>(el.func_type));
	llvm::FunctionType* ft = llvm::FunctionType::get(lookup_type(static_cast<int>( el.func_type)), args_v, false);

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
	llvm::Value* v=nullptr;
	for (auto& stat : el.statement_list) {
			v= stat->accept(*this);
	}
	return v;
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

llvm::Value* CodeGenVisitor::visit(VariableExprAST& el) {
	return nullptr;
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
