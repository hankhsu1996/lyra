#pragma once

// Central dispatch contract for the AST-to-HIR expression layer.
//
// ADMISSION RULE: This header declares ONLY the two recursive dispatchers
// (`LowerProcExpr` and `LowerStructuralExpr`). Anything else has a clearer
// home:
//   - Subsystem-internal helpers       -> the subsystem's own .hpp
//   - Operations on lowerer / module   -> methods on the respective class
//   - Pure HIR primitives              -> inline at call sites
//   - Shared between 2-3 subsystems    -> put in one subsystem's .hpp; the
//                                         other subsystem #includes it directly
//
// A new entry added here must answer: "Why am I a dispatcher?" If the answer
// is anything else, this is the wrong file.
//
// Only included by expression/*.cpp.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Procedural-context expression dispatcher. Every per-kind handler in the
// expression layer recurses through this entry.
auto LowerProcExpr(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

// Structural-context expression dispatcher. Same role, for continuous-assign
// and structural-var-initializer contexts.
auto LowerStructuralExpr(
    ScopeLowerer& scope, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
