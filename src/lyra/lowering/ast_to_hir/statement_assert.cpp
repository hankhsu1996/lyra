#include "lyra/lowering/ast_to_hir/statement_assert.hpp"

#include <format>
#include <optional>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
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

auto MapImmediateAssertionKind(slang::ast::AssertionKind kind)
    -> std::optional<hir::ImmediateAssertionKind> {
  switch (kind) {
    case slang::ast::AssertionKind::Assert:
      return hir::ImmediateAssertionKind::kAssert;
    case slang::ast::AssertionKind::Assume:
      return hir::ImmediateAssertionKind::kAssume;
    case slang::ast::AssertionKind::CoverProperty:
      return hir::ImmediateAssertionKind::kCover;
    default:
      return std::nullopt;
  }
}

}  // namespace

auto LowerImmediateAssertionStatement(
    const slang::ast::ImmediateAssertionStatement& assert_stmt,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, assert_stmt.sourceRange);

  if (assert_stmt.isFinal) {
    env.ctx->sink->Error(
        env.span,
        "deferred final immediate assertions are unsupported; "
        "pass --disable-assertions to skip");
    return hir::kInvalidStatementId;
  }

  auto kind = MapImmediateAssertionKind(assert_stmt.assertionKind);
  if (!kind.has_value()) {
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

  if (*kind == hir::ImmediateAssertionKind::kCover && fail_action.has_value()) {
    throw common::InternalError(
        "LowerImmediateAssertionStatement",
        "immediate cover must not carry fail_action");
  }

  auto timing = assert_stmt.isDeferred
                    ? hir::ImmediateAssertionTiming::kObservedDeferred
                    : hir::ImmediateAssertionTiming::kSimple;

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kImmediateAssertion,
          .span = env.span,
          .data = hir::ImmediateAssertionStatementData{
              .kind = *kind,
              .timing = timing,
              .condition = condition,
              .pass_action = pass_action,
              .fail_action = fail_action}});
}

}  // namespace lyra::lowering::ast_to_hir
