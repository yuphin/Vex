#pragma once
#include "Visitor.h"
#include "AST.h"

namespace Vex {
	struct ASTPayload {
		Type* ty_info;
		obj_type s_type;
		yy::location loc;
		ASTPayload(const obj_type& s_type,const yy::location& loc) : ty_info(nullptr), 
			s_type(s_type), loc(loc) {}
		ASTPayload(Type* ty_info,const yy::location& loc) : ty_info(ty_info),loc(loc) {}
		ASTPayload(Type* ty_info, obj_type s_type) : ty_info(ty_info), s_type(s_type) {}

		bool operator<(const ASTPayload& rhs);
		obj_type get_basic_type() const;
	};

}
