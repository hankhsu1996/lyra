#include "lyra/lowering/ast_to_hir/statement_termination.hpp"

#include <cstddef>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto TryLowerFinishStopExit(
    const slang::ast::CallExpression& call,
    const TerminationFunctionInfo& term_info, StatementLoweringEnv& env)
    -> std::optional<hir::StatementId> {
  int level = term_info.default_level;
  if (!call.arguments().empty()) {
    const slang::ast::Expression& arg = *call.arguments()[0];
    if (arg.kind == slang::ast::ExpressionKind::IntegerLiteral) {
      const auto& literal = arg.as<slang::ast::IntegerLiteral>();
      auto val = literal.getValue();
      if (val.hasUnknown()) {
        env.ctx->sink->Error(
            env.span, "termination level cannot contain X or Z");
        return hir::kInvalidStatementId;
      }
      level = static_cast<int>(val.as<int>().value());
    } else {
      env.ctx->sink->Error(
          env.span, "termination level must be a constant integer");
      return hir::kInvalidStatementId;
    }
  }

  hir::TerminationKind kind = hir::TerminationKind::kFinish;
  switch (term_info.type) {
    case TerminationType::kFinish:
      kind = hir::TerminationKind::kFinish;
      break;
    case TerminationType::kStop:
      kind = hir::TerminationKind::kStop;
      break;
    case TerminationType::kExit:
      kind = hir::TerminationKind::kExit;
      break;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kTerminate,
          .span = env.span,
          .data =
              hir::TerminateStatementData{
                  .kind = kind, .level = level, .message_args = {}},
      });
}

auto TryLowerFatal(
    const slang::ast::CallExpression& call, StatementLoweringEnv& env)
    -> std::optional<hir::StatementId> {
  // $fatal([finish_number], [fmt, args...])
  // First arg is optional finish_number (integer literal),
  // remaining args are format string and values.
  int level = 1;
  size_t message_start = 0;

  if (!call.arguments().empty()) {
    const slang::ast::Expression& first_arg = *call.arguments()[0];
    if (first_arg.kind == slang::ast::ExpressionKind::IntegerLiteral) {
      const auto& literal = first_arg.as<slang::ast::IntegerLiteral>();
      auto val = literal.getValue();
      if (val.hasUnknown()) {
        env.ctx->sink->Error(env.span, "$fatal level cannot contain X or Z");
        return hir::kInvalidStatementId;
      }
      level = static_cast<int>(val.as<int>().value());
      message_start = 1;
    } else if (first_arg.kind == slang::ast::ExpressionKind::StringLiteral) {
      message_start = 0;
    } else if (first_arg.type->isIntegral()) {
      env.ctx->sink->Error(
          env.span, "$fatal level must be a constant integer literal");
      return hir::kInvalidStatementId;
    } else {
      message_start = 0;
    }
  }

  std::vector<hir::ExpressionId> message_args;
  for (size_t i = message_start; i < call.arguments().size(); ++i) {
    hir::ExpressionId arg_id = env.LowerExpr(*call.arguments()[i]);
    if (!arg_id) {
      return hir::kInvalidStatementId;
    }
    message_args.push_back(arg_id);
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kTerminate,
          .span = env.span,
          .data =
              hir::TerminateStatementData{
                  .kind = hir::TerminationKind::kFatal,
                  .level = level,
                  .message_args = std::move(message_args)},
      });
}

}  // namespace

auto TryLowerTerminationCall(
    const slang::ast::ExpressionStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  if (stmt.expr.kind != slang::ast::ExpressionKind::Call) {
    return std::nullopt;
  }

  const auto& call = stmt.expr.as<slang::ast::CallExpression>();
  if (!call.isSystemCall()) {
    return std::nullopt;
  }

  std::string_view name = call.getSubroutineName();
  const SystemFunctionInfo* info = FindSystemFunction(name);
  if (info == nullptr) {
    return std::nullopt;
  }

  StatementLoweringEnv env(lowerer, stmt.sourceRange);

  if (const auto* term_info =
          std::get_if<TerminationFunctionInfo>(&info->payload)) {
    return TryLowerFinishStopExit(call, *term_info, env);
  }

  if (std::get_if<FatalFunctionInfo>(&info->payload) != nullptr) {
    return TryLowerFatal(call, env);
  }

  return std::nullopt;
}

}  // namespace lyra::lowering::ast_to_hir
