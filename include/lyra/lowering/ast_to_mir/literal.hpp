#pragma once

#include "lyra/common/literal.hpp"

namespace slang::ast {
class IntegerLiteral;
class RealLiteral;
class StringLiteral;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST IntegerLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::IntegerLiteral& literal) -> common::Literal;

// Lowers a slang AST StringLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal;

// Lowers a slang AST RealLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Literal;

}  // namespace lyra::lowering::ast_to_mir
