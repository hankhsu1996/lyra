#include "lyra/lowering/ast_to_hir/statement_assert.hpp"

#include <format>
#include <optional>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lower an optional child statement with three-state propagation:
// nullptr -> nullopt, child error -> kInvalidStatementId, success -> valid id.
auto LowerOptionalChildStatement(
    const slang::ast::Statement* child, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  if (child == nullptr) return std::nullopt;
  auto result = LowerStatement(*child, lowerer);
  if (!result.has_value()) return std::nullopt;
  if (!*result) return hir::kInvalidStatementId;
  return *result;
}

}  // namespace

auto LowerImmediateAssertionStatement(
    const slang::ast::ImmediateAssertionStatement& assert_stmt,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, assert_stmt.sourceRange);

  if (assert_stmt.isDeferred || assert_stmt.isFinal) {
    env.ctx->sink->Error(
        env.span,
        "deferred immediate assertions are unsupported; "
        "pass --disable-assertions to skip");
    return hir::kInvalidStatementId;
  }

  if (assert_stmt.assertionKind != slang::ast::AssertionKind::Assert) {
    env.ctx->sink->Error(
        env.span, std::format(
                      "immediate '{}' is unsupported; "
                      "pass --disable-assertions to skip",
                      toString(assert_stmt.assertionKind)));
    return hir::kInvalidStatementId;
  }

  hir::ExpressionId condition = env.LowerExpr(assert_stmt.cond);
  if (!condition) {
    return hir::kInvalidStatementId;
  }

  auto pass_action = LowerOptionalChildStatement(assert_stmt.ifTrue, lowerer);
  if (pass_action.has_value() && !*pass_action) return hir::kInvalidStatementId;

  auto fail_action = LowerOptionalChildStatement(assert_stmt.ifFalse, lowerer);
  if (fail_action.has_value() && !*fail_action) return hir::kInvalidStatementId;

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kImmediateAssertion,
          .span = env.span,
          .data = hir::ImmediateAssertionStatementData{
              .kind = hir::ImmediateAssertionKind::kAssert,
              .condition = condition,
              .pass_action = pass_action,
              .fail_action = fail_action}});
}

}  // namespace lyra::lowering::ast_to_hir
