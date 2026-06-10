#pragma once

// Central dispatch contract for the AST-to-HIR statement layer.
//
// ADMISSION RULE: This header declares ONLY the central statement dispatcher.
// Anything else has a clearer home:
//   - Subsystem-internal helpers       -> the subsystem's own .hpp
//   - Operations on lowerer / module   -> methods on the respective class
//   - Pure HIR primitives              -> inline at call sites
//   - Shared between 2-3 subsystems    -> put in one subsystem's .hpp; the
//                                         other subsystem #includes it directly
//
// A new entry added here must answer: "Why am I a dispatcher?" If the answer
// is anything else, this is the wrong file.
//
// Only included by statement/*.cpp.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Statement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Central statement dispatcher. Every per-kind handler in the statement layer
// recurses through this entry to lower nested statements.
auto LowerStatement(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Statement& stmt)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
