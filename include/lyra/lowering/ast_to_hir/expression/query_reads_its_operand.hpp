#pragma once

namespace slang::ast {
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `call` is an LRM 20.6 / 20.7 query whose answer is read from its
// operand's value, as a dynamically sized value's extent is: what such a query
// answers changes when that value does, so whatever is sensitive to the call is
// sensitive to the operand (LRM 9.4.2). False for any other call, and for a
// query whose answer the operand's type fixes.
[[nodiscard]] auto QueryReadsItsOperand(const slang::ast::CallExpression& call)
    -> bool;

}  // namespace lyra::lowering::ast_to_hir
