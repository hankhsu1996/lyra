#include "lyra/lowering/ast_to_hir/statement_assert.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <variant>
#include <vector>

#include <slang/ast/statements/MiscStatements.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/deferred_assertion.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/callable_signature_query.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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

// --- Single legality gate for ref capture targets ---
// Recursively peels HIR expression projections until reaching a root.
// Only fully-validated projections are stored (constant indices resolved).

auto ExtractHirCaptureTarget(
    hir::ExpressionId expr_id, const hir::Arena& arena,
    const SymbolTable& symbols, const ConstantArena& constants,
    SourceSpan diag_span) -> std::optional<hir::HirCaptureTarget> {
  const auto& expr = arena[expr_id];

  if (const auto* name_ref =
          std::get_if<hir::NameRefExpressionData>(&expr.data)) {
    const auto& sym = symbols[name_ref->symbol];
    return hir::HirCaptureTarget{
        .root_symbol = name_ref->symbol,
        .root_type = sym.type,
        .path = {},
        .result_type = expr.type,
        .span = diag_span,
        .source_expr = expr_id,
    };
  }

  if (const auto* hier =
          std::get_if<hir::HierarchicalRefExpressionData>(&expr.data)) {
    const auto& sym = symbols[hier->target];
    return hir::HirCaptureTarget{
        .root_symbol = hier->target,
        .root_type = sym.type,
        .path = {},
        .result_type = expr.type,
        .span = diag_span,
        .source_expr = expr_id,
    };
  }

  if (const auto* member =
          std::get_if<hir::MemberAccessExpressionData>(&expr.data)) {
    auto base = ExtractHirCaptureTarget(
        member->base, arena, symbols, constants, diag_span);
    if (!base) return std::nullopt;
    base->path.emplace_back(
        hir::MemberProjection{
            .field = FieldId{static_cast<uint32_t>(member->field_index)},
        });
    base->result_type = expr.type;
    base->source_expr = expr_id;
    return base;
  }

  if (const auto* elem =
          std::get_if<hir::ElementAccessExpressionData>(&expr.data)) {
    // Validate index is a compile-time constant (deferred-ref legality).
    const auto& index_expr = arena[elem->index];
    const auto* const_data =
        std::get_if<hir::ConstantExpressionData>(&index_expr.data);
    if (const_data == nullptr) {
      return std::nullopt;
    }
    const Constant& constant = constants[const_data->constant];
    if (!std::holds_alternative<IntegralConstant>(constant.value)) {
      return std::nullopt;
    }
    // Index is constant. Store the validated ExpressionId in the shared
    // IndexProjection. The constant-ness was checked above; downstream
    // MIR lowering can rely on this being a constant expression.
    auto base = ExtractHirCaptureTarget(
        elem->base, arena, symbols, constants, diag_span);
    if (!base) return std::nullopt;
    base->path.emplace_back(
        hir::IndexProjection{
            .index = elem->index,
        });
    base->result_type = expr.type;
    base->source_expr = expr_id;
    return base;
  }

  return std::nullopt;
}

// --- Deferred action branch extraction ---
// Centralizes slang action-shape validation + HIR call extraction into
// one normalized result, so the dual extraction invariant is not spread.

// (Branch validation moved to LowerAndNormalizeActionBranch in statement.cpp)

// Build a DeferredAction from HIR callable signature and HIR args.
auto BuildHirDeferredAction(
    SymbolId callee_symbol, const hir::HirCallableSignature& sig,
    const hir::CallExpressionData& hir_call, const hir::Arena& arena,
    Context& ctx, SourceSpan span) -> std::optional<hir::DeferredAction> {
  if (sig.return_type != ctx.VoidType()) {
    ctx.sink->Unsupported(
        span, "deferred assertion action subroutine must be void",
        UnsupportedCategory::kFeature);
    return std::nullopt;
  }

  if (sig.params.size() != hir_call.arguments.size()) {
    throw common::InternalError(
        "BuildHirDeferredAction",
        std::format(
            "callable param count {} != HIR argument count {}",
            sig.params.size(), hir_call.arguments.size()));
  }

  std::vector<hir::DeferredCapture> captures;
  captures.reserve(sig.params.size());

  for (size_t i = 0; i < sig.params.size(); ++i) {
    const auto& param = sig.params[i];

    switch (param.direction) {
      case ParameterDirection::kInput: {
        const auto& type = (*ctx.type_arena)[param.type];
        auto kind = type.Kind();
        if (kind == TypeKind::kString || kind == TypeKind::kDynamicArray ||
            kind == TypeKind::kQueue || kind == TypeKind::kAssociativeArray) {
          ctx.sink->Unsupported(
              span,
              std::format(
                  "deferred assertion action parameter {} has managed "
                  "type (string/queue/dynamic array not supported)",
                  i),
              UnsupportedCategory::kFeature);
          return std::nullopt;
        }
        captures.emplace_back(
            hir::ValueCapture{
                .expr = hir_call.arguments[i],
                .type = param.type,
            });
        break;
      }

      case ParameterDirection::kRef: {
        auto target = ExtractHirCaptureTarget(
            hir_call.arguments[i], arena, *ctx.symbol_table,
            *ctx.active_constant_arena, span);
        if (!target) {
          ctx.sink->Unsupported(
              span,
              std::format(
                  "deferred assertion action parameter {} has unsupported "
                  "ref-capture expression form",
                  i),
              UnsupportedCategory::kFeature);
          return std::nullopt;
        }
        captures.emplace_back(
            hir::RefCapture{
                .target = std::move(*target),
                .type = param.type,
            });
        break;
      }

      case ParameterDirection::kConstRef: {
        auto target = ExtractHirCaptureTarget(
            hir_call.arguments[i], arena, *ctx.symbol_table,
            *ctx.active_constant_arena, span);
        if (!target) {
          ctx.sink->Unsupported(
              span,
              std::format(
                  "deferred assertion action parameter {} has unsupported "
                  "const-ref-capture expression form",
                  i),
              UnsupportedCategory::kFeature);
          return std::nullopt;
        }
        captures.emplace_back(
            hir::ConstRefCapture{
                .target = std::move(*target),
                .type = param.type,
            });
        break;
      }

      case ParameterDirection::kOutput:
      case ParameterDirection::kInOut:
        ctx.sink->Unsupported(
            span,
            std::format(
                "deferred assertion action parameter {} has out/inout "
                "direction (not supported for deferred assertions)",
                i),
            UnsupportedCategory::kFeature);
        return std::nullopt;
    }
  }

  return hir::DeferredAction{
      .callee_symbol = callee_symbol,
      .captures = std::move(captures),
  };
}

// --- Tri-state branch result for deferred action construction ---

enum class DeferredBranchStatus {
  kAbsent,   // no branch present in source
  kBuilt,    // action successfully constructed
  kInvalid,  // branch present but unsupported/invalid (diagnostic emitted)
};

struct DeferredBranchResult {
  DeferredBranchStatus status = DeferredBranchStatus::kAbsent;
  std::optional<hir::DeferredAction> action;
};

auto TryBuildBranchAction(
    const LoweredActionBranch& branch, const hir::Arena& arena, Context& ctx,
    SourceSpan span) -> DeferredBranchResult {
  switch (branch.kind) {
    case LoweredActionBranchKind::kAbsent:
      return {.status = DeferredBranchStatus::kAbsent, .action = std::nullopt};
    case LoweredActionBranchKind::kInvalid:
      return {.status = DeferredBranchStatus::kInvalid, .action = std::nullopt};
    case LoweredActionBranchKind::kValidSingleCall:
      break;
  }

  SymbolId callee_symbol = branch.hir_call->callee;
  if (!callee_symbol) {
    ctx.sink->Error(span, "undefined callee in deferred assertion action");
    return {.status = DeferredBranchStatus::kInvalid, .action = std::nullopt};
  }

  if (ctx.callable_signatures == nullptr) {
    throw common::InternalError(
        "TryBuildBranchAction", "no callable signature table");
  }
  const auto* sig = GetHirCallableSignature(
      callee_symbol, *ctx.callable_signatures, *ctx.sink, span);
  if (sig == nullptr) {
    return {.status = DeferredBranchStatus::kInvalid, .action = std::nullopt};
  }

  auto action = BuildHirDeferredAction(
      callee_symbol, *sig, *branch.hir_call, arena, ctx, span);
  if (!action) {
    return {.status = DeferredBranchStatus::kInvalid, .action = std::nullopt};
  }

  return {
      .status = DeferredBranchStatus::kBuilt,
      .action = std::move(*action),
  };
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

  // Simple immediate assertion: lower children and produce
  // ImmediateAssertionStatementData.
  if (!assert_stmt.isDeferred) {
    auto pass_action = LowerOptionalChildStatement(assert_stmt.ifTrue, lowerer);
    if (pass_action.has_value() && !*pass_action) {
      return hir::kInvalidStatementId;
    }
    auto fail_action =
        LowerOptionalChildStatement(assert_stmt.ifFalse, lowerer);
    if (fail_action.has_value() && !*fail_action) {
      return hir::kInvalidStatementId;
    }
    if (*kind == hir::ImmediateAssertionKind::kCover &&
        fail_action.has_value()) {
      throw common::InternalError(
          "LowerImmediateAssertionStatement",
          "immediate cover must not carry fail_action");
    }
    return env.ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kImmediateAssertion,
            .span = env.span,
            .data = hir::ImmediateAssertionStatementData{
                .kind = *kind,
                .condition = condition,
                .pass_action = pass_action,
                .fail_action = fail_action,
            }});
  }

  // Observed deferred assertion: normalize action branches, build
  // semantic DeferredAction content, produce DeferredAssertionStatementData.
  // The statement directly owns its deferred action semantic content.
  // For observed deferred timing, HIR must produce a fully valid semantic
  // node or reject the entire assertion statement. No partial construction.
  const auto& arena = *env.ctx->hir_arena;
  bool is_cover = *kind == hir::ImmediateAssertionKind::kCover;

  auto pass_branch = LowerAndNormalizeActionBranch(
      assert_stmt.ifTrue, lowerer, arena, env.span, "pass action");
  auto fail_branch = LowerAndNormalizeActionBranch(
      assert_stmt.ifFalse, lowerer, arena, env.span, "fail action");

  if (is_cover && fail_branch.kind != LoweredActionBranchKind::kAbsent) {
    throw common::InternalError(
        "LowerImmediateAssertionStatement",
        "deferred cover must not carry fail_action");
  }

  bool has_default_fail_report = false;
  std::optional<hir::DeferredAction> deferred_fail_action;
  std::optional<hir::DeferredAction> deferred_pass_action;

  if (is_cover) {
    auto pass_result =
        TryBuildBranchAction(pass_branch, arena, *env.ctx, env.span);
    switch (pass_result.status) {
      case DeferredBranchStatus::kBuilt:
        deferred_pass_action = std::move(pass_result.action);
        break;
      case DeferredBranchStatus::kAbsent:
        break;
      case DeferredBranchStatus::kInvalid:
        return hir::kInvalidStatementId;
    }
  } else {
    auto fail_result =
        TryBuildBranchAction(fail_branch, arena, *env.ctx, env.span);
    switch (fail_result.status) {
      case DeferredBranchStatus::kBuilt:
        deferred_fail_action = std::move(fail_result.action);
        break;
      case DeferredBranchStatus::kAbsent:
        has_default_fail_report = true;
        break;
      case DeferredBranchStatus::kInvalid:
        return hir::kInvalidStatementId;
    }

    auto pass_result =
        TryBuildBranchAction(pass_branch, arena, *env.ctx, env.span);
    switch (pass_result.status) {
      case DeferredBranchStatus::kBuilt:
        deferred_pass_action = std::move(pass_result.action);
        break;
      case DeferredBranchStatus::kAbsent:
        break;
      case DeferredBranchStatus::kInvalid:
        return hir::kInvalidStatementId;
    }
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kDeferredAssertion,
          .span = env.span,
          .data = hir::DeferredAssertionStatementData{
              .kind = *kind,
              .condition = condition,
              .pass_action = pass_branch.statement_id,
              .fail_action = fail_branch.statement_id,
              .has_default_fail_report = has_default_fail_report,
              .deferred_fail_action = std::move(deferred_fail_action),
              .deferred_pass_action = std::move(deferred_pass_action),
          }});
}

}  // namespace lyra::lowering::ast_to_hir
