#include "lyra/lowering/ast_to_hir/net_overlay.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/ValueSymbol.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The runs one operand of a side names. A concatenation names its own operands'
// runs in the order written, which is most significant first, so nesting one
// inside another needs nothing of its own. Anything else is a net or a constant
// select of one, which is its own longest static prefix (LRM 11.5.3) and names
// the part of the net its one run covers whole.
auto RunsOf(
    StructuralScopeLowerer& scope, slang::ast::EvalContext& eval_context,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<NetSide> {
  if (expr.kind == slang::ast::ExpressionKind::Concatenation) {
    NetSide runs;
    for (const slang::ast::Expression* operand :
         expr.as<slang::ast::ConcatenationExpression>().operands()) {
      auto named = RunsOf(scope, eval_context, *operand, span, code, frame);
      if (!named) return std::unexpected(std::move(named.error()));
      runs.insert(runs.end(), named->begin(), named->end());
    }
    return runs;
  }
  const slang::ast::ValuePath path(expr, eval_context);
  const slang::ast::ValueSymbol* root = path.rootSymbol();
  if (path.lsp != &expr || path.rootExpr == nullptr || root == nullptr ||
      root->kind != slang::ast::SymbolKind::Net) {
    return diag::Fail(
        span, code,
        "an operand that is neither a net nor a constant select of one "
        "(LRM 10.11) is not yet supported where a connection or an alias "
        "names a run of positions");
  }
  // A run is counted in the positions of a bit vector. A net whose data type is
  // an unpacked aggregate resolves per bit as well (LRM 6.7.1), but it keeps no
  // runs of positions another net could join, whether the whole of it is named
  // or one element.
  if (!root->getType().isIntegral()) {
    return diag::Fail(
        span, code,
        "a net whose data type is an unpacked aggregate is not yet supported "
        "where a connection or an alias names a run of positions");
  }
  auto part = scope.LowerExpr(expr, frame);
  if (!part) return std::unexpected(std::move(part.error()));
  return NetSide{NetRun{
      .part = frame.Exprs().Add(*std::move(part)),
      .offset = 0,
      .width = static_cast<std::uint32_t>(
          path.lspBounds.second - path.lspBounds.first + 1)}};
}

}  // namespace

auto NetRunsOfLvalue(
    StructuralScopeLowerer& scope, const slang::ast::Symbol& eval_scope,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<NetSide> {
  slang::ast::EvalContext eval_context(eval_scope);
  return RunsOf(scope, eval_context, expr, span, code, frame);
}

auto CoupleSides(
    const NetSide& left, const NetSide& right, diag::SourceSpan span)
    -> std::vector<hir::NetJoin> {
  std::vector<hir::NetJoin> couplings;
  std::size_t at_left = 0;
  std::size_t at_right = 0;
  std::uint32_t taken_left = 0;
  std::uint32_t taken_right = 0;
  while (at_left < left.size() && at_right < right.size()) {
    const NetRun& here = left[at_left];
    const NetRun& there = right[at_right];
    const std::uint32_t run =
        std::min(here.width - taken_left, there.width - taken_right);
    couplings.push_back(
        hir::NetJoin{
            .span = span,
            .here = here.part,
            .here_offset = here.offset + here.width - taken_left - run,
            .there = there.part,
            .there_offset = there.offset + there.width - taken_right - run,
            .width = run});
    taken_left += run;
    taken_right += run;
    if (taken_left == here.width) {
      ++at_left;
      taken_left = 0;
    }
    if (taken_right == there.width) {
      ++at_right;
      taken_right = 0;
    }
  }
  if (at_left != left.size() || at_right != right.size()) {
    throw InternalError(
        "CoupleSides: the two sides of one statement cover the same number of "
        "positions, which the front end requires of every construct that "
        "forms an overlay");
  }
  return couplings;
}

}  // namespace lyra::lowering::ast_to_hir
