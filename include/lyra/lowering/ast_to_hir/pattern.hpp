#pragma once

// Pattern lowering (LRM 12.6). Turns a slang `Pattern` tree into HIR
// `Pattern` nodes. An identifier a pattern binds is declared by the
// `VariablePattern` node that names it, so a reference to it resolves to that
// node's `PatternId`; the unit records the slang symbol under that id for
// reference resolution to find.

#include <optional>
#include <vector>

#include <slang/ast/Patterns.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowers one slang pattern into the enclosing scope's pattern arena and
// returns the slot it landed in. When the pattern declares an identifier, the
// unit records the slang symbol under that same slot, which is the identity a
// reference to the identifier carries -- storing and recording are one step so
// that no id can exist without the declaration a later lookup expects.
//
// A pattern's meaning does not depend on whether a procedural body or a
// structural scope encloses it: LRM 12.6 puts every identifier a pattern binds
// in the pattern's own scope, so nothing here reaches for a declaration arena.
// One template over the pass class serves both contexts.
//
// `subject_type` is the type of the value the pattern is matched against; the
// node records it, so a consumer descending the tree does not walk the
// subject's type alongside the pattern.
template <ExprLowerer Lowerer>
auto AddPattern(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Pattern& pattern,
    const slang::ast::Type& subject_type, diag::SourceSpan span)
    -> diag::Result<hir::PatternId>;

// Lowers a whole predicate clause sequence. One loop for every `if` / `?:`
// in either context: the plain LRM 12.4 predicate is the one-clause,
// pattern-free case of the same sequence, not a separate shape.
template <ExprLowerer Lowerer, typename Conditions>
auto LowerConditionClauses(
    Lowerer& lowerer, WalkFrame frame, const Conditions& conditions,
    diag::SourceSpan span) -> diag::Result<std::vector<hir::ConditionClause>> {
  std::vector<hir::ConditionClause> clauses;
  clauses.reserve(conditions.size());
  for (const auto& condition : conditions) {
    auto expr_or = lowerer.LowerExpr(*condition.expr, frame);
    if (!expr_or) return std::unexpected(std::move(expr_or.error()));
    const hir::ExprId expr_id = frame.Exprs().Add(*std::move(expr_or));

    std::optional<hir::PatternId> pattern_id;
    if (condition.pattern != nullptr) {
      auto pattern_or = AddPattern(
          lowerer, frame, *condition.pattern, *condition.expr->type, span);
      if (!pattern_or) return std::unexpected(std::move(pattern_or.error()));
      pattern_id = *pattern_or;
    }

    clauses.push_back(
        hir::ConditionClause{.expr = expr_id, .pattern = pattern_id});
  }
  return clauses;
}

}  // namespace lyra::lowering::ast_to_hir
