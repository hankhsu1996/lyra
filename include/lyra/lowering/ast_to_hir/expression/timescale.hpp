#pragma once

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The timescale system functions and task (LRM 20.4), when `call` is one of
// them. Their argument names a design element rather than standing for a
// value, and everything they answer is fixed at elaboration, so each is
// resolved ahead of the argument loop: `$timeunit` and `$timeprecision` are
// the constants the front end evaluates them to (LRM 11.2.1), and
// `$printtimescale` writes the line made of the element's name and the same
// two values. Returns nullopt when the call is none of them.
auto LowerTimescaleCall(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>>;

}  // namespace lyra::lowering::ast_to_hir
