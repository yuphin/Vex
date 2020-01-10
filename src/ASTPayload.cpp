#include "ASTPayload.h"

namespace Vex {
	bool ASTPayload::operator<(const ASTPayload& rhs) {
		auto lhs_t = (*this).get_basic_type();
		auto rhs_t = rhs.get_basic_type();

		return lhs_t < rhs_t;
	}

	obj_type ASTPayload::get_basic_type() const {
		if (this->ty_info) {
			return this->ty_info->s_type;
		} else {
			return this->s_type;
		}
	}
}

