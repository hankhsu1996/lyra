#pragma once

#include "lyra/common/literal.hpp"

namespace slang::ast {
class IntegerLiteral;
class StringLiteral;
}  // namespace slang::ast

namespace lyra::lowering {

// Lowers a slang AST IntegerLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::IntegerLiteral& literal) -> common::Literal;

// Lowers a slang AST StringLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal;

}  // namespace lyra::lowering
