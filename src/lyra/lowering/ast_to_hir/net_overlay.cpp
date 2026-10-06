#include "lyra/lowering/ast_to_hir/net_overlay.hpp"

#include <cstdint>
#include <expected>
#include <utility>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/ValueSymbol.h>

#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The positions one operand of a side names. A concatenation names its own
// operands' positions in the order written, which is most significant first,
// so nesting one inside another needs nothing of its own. Anything else is a
// net or a constant select of one, which is its own longest static prefix (LRM
// 11.5.3) and names the part of the net whose positions it covers whole.
auto PositionsOf(
    StructuralScopeLowerer& scope, slang::ast::EvalContext& eval_context,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<hir::NetSide> {
  if (expr.kind == slang::ast::ExpressionKind::Concatenation) {
    hir::NetSide operands;
    for (const slang::ast::Expression* operand :
         expr.as<slang::ast::ConcatenationExpression>().operands()) {
      auto named =
          PositionsOf(scope, eval_context, *operand, span, code, frame);
      if (!named) return std::unexpected(std::move(named.error()));
      operands.insert(operands.end(), named->begin(), named->end());
    }
    return operands;
  }
  const slang::ast::ValuePath path(expr, eval_context);
  const slang::ast::ValueSymbol* root = path.rootSymbol();
  if (path.lsp != &expr || path.rootExpr == nullptr || root == nullptr ||
      root->kind != slang::ast::SymbolKind::Net) {
    return diag::Fail(
        span, code,
        "an operand that is neither a net nor a constant select of one "
        "(LRM 10.11) is not yet supported where a connection or an alias "
        "names some of a net's bits");
  }
  // Positions are counted in the bits of a vector. A net whose data type is an
  // unpacked aggregate resolves per bit as well (LRM 6.7.1), but it keeps no
  // positions another net could join, whether the whole of it is named or one
  // element.
  if (!root->getType().isIntegral()) {
    return diag::Fail(
        span, code,
        "a net whose data type is an unpacked aggregate is not yet supported "
        "where a connection or an alias names some of a net's bits");
  }
  auto part = scope.LowerExpr(expr, frame);
  if (!part) return std::unexpected(std::move(part.error()));
  return hir::NetSide{hir::NetPositions{
      .part = frame.Exprs().Add(*std::move(part)),
      .offset = 0,
      .width = static_cast<std::uint32_t>(
          path.lspBounds.second - path.lspBounds.first + 1)}};
}

}  // namespace

auto NetPositionsOfLvalue(
    StructuralScopeLowerer& scope, const slang::ast::Symbol& eval_scope,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<hir::NetSide> {
  slang::ast::EvalContext eval_context(eval_scope);
  return PositionsOf(scope, eval_context, expr, span, code, frame);
}

}  // namespace lyra::lowering::ast_to_hir
