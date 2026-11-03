#pragma once

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ArbitrarySymbolExpression;
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `call` is one of the assertion control tasks, whose arguments after
// the integers name scopes and assertions rather than values (LRM 20.11).
[[nodiscard]] auto IsAssertionControlCall(
    const slang::ast::CallExpression& call) -> bool;

// One scope or assertion an assertion control task's list names (LRM 20.11):
// the object of an instance or a generate block, or the extent of a named
// block, a statement label's or a subroutine's. A scope no statement the
// tasks control can stand in -- a clocking block, a declaration of a
// sequence, a type -- selects nothing, and is answered with nothing.
[[nodiscard]] auto LowerAssertionControlTarget(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>>;

}  // namespace lyra::lowering::ast_to_hir
