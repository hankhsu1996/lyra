#include "lyra/lowering/ast_to_hir/routine.hpp"

#include <optional>

#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ConvertProcessKind(slang::ast::ProceduralBlockKind kind)
    -> hir::ProcessKind {
  using SlangKind = slang::ast::ProceduralBlockKind;
  using HirKind = hir::ProcessKind;

  switch (kind) {
    case SlangKind::Initial:
      return HirKind::kInitial;
    case SlangKind::Final:
      return HirKind::kFinal;
    case SlangKind::Always:
      return HirKind::kAlways;
    case SlangKind::AlwaysComb:
      return HirKind::kAlwaysComb;
    case SlangKind::AlwaysLatch:
      return HirKind::kAlwaysLatch;
    case SlangKind::AlwaysFF:
      return HirKind::kAlwaysFf;
  }
  throw common::InternalError(
      "routine lowering", "unknown procedural block kind");
}

}  // namespace

auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& proc, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ProcessId {
  SourceSpan span = ctx->SpanOf(GetSourceRange(proc));
  hir::ProcessKind kind = ConvertProcessKind(proc.procedureKind);

  std::optional<hir::StatementId> body_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kBlock);
    body_result = LowerStatement(proc.getBody(), registrar, ctx);
  }

  hir::StatementId body;
  if (!body_result.has_value()) {
    // Empty body (e.g., `initial;`) - create empty block
    body = ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kBlock,
            .span = span,
            .data = hir::BlockStatementData{.statements = {}},
        });
  } else if (!*body_result) {
    return hir::kInvalidProcessId;
  } else {
    body = *body_result;
  }

  return ctx->hir_arena->AddProcess(
      hir::Process{
          .kind = kind,
          .span = span,
          .body = body,
      });
}

auto LowerFunction(
    const slang::ast::SubroutineSymbol& func, SymbolRegistrar& registrar,
    Context* ctx) -> hir::FunctionId {
  SourceSpan span = ctx->SpanOf(GetSourceRange(func));

  // Reject non-integral/void return types
  const auto& ret_type = func.getReturnType();
  if (!ret_type.isIntegral() && !ret_type.isVoid()) {
    ctx->sink->Error(span, "only integral or void return types supported");
    return hir::kInvalidFunctionId;
  }

  TypeId return_type = LowerType(ret_type, span, ctx);
  if (!return_type) {
    return hir::kInvalidFunctionId;
  }

  // Check parameter directions before registering function symbol
  for (const slang::ast::FormalArgumentSymbol* arg : func.getArguments()) {
    if (arg->direction != slang::ast::ArgumentDirection::In) {
      ctx->sink->Error(span, "only input parameters supported");
      return hir::kInvalidFunctionId;
    }
  }

  // Check if symbol was pre-registered (two-phase lowering)
  SymbolId symbol = registrar.Lookup(func);
  if (!symbol) {
    symbol = registrar.Register(func, SymbolKind::kFunction, return_type);
  }

  std::vector<SymbolId> parameters;
  std::optional<hir::StatementId> body_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kFunction);

    for (const slang::ast::FormalArgumentSymbol* arg : func.getArguments()) {
      TypeId arg_type = LowerType(arg->getType(), span, ctx);
      if (!arg_type) {
        return hir::kInvalidFunctionId;
      }
      SymbolId arg_sym =
          registrar.Register(*arg, SymbolKind::kParameter, arg_type);
      parameters.push_back(arg_sym);
    }

    body_result = LowerStatement(func.getBody(), registrar, ctx);
  }

  hir::StatementId body;
  if (!body_result.has_value()) {
    // Empty body - create empty block
    body = ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kBlock,
            .span = span,
            .data = hir::BlockStatementData{.statements = {}},
        });
  } else if (!*body_result) {
    return hir::kInvalidFunctionId;
  } else {
    body = *body_result;
  }

  return ctx->hir_arena->AddFunction(
      hir::Function{
          .symbol = symbol,
          .span = span,
          .return_type = return_type,
          .parameters = std::move(parameters),
          .body = body,
      });
}

auto LowerTask(
    const slang::ast::SubroutineSymbol& task, SymbolRegistrar& registrar,
    Context* ctx) -> hir::TaskId {
  SourceSpan span = ctx->SpanOf(GetSourceRange(task));

  SymbolId symbol = registrar.Register(task, SymbolKind::kTask, kInvalidTypeId);

  std::vector<SymbolId> parameters;
  std::optional<hir::StatementId> body_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kTask);

    for (const slang::ast::FormalArgumentSymbol* arg : task.getArguments()) {
      TypeId arg_type = LowerType(arg->getType(), span, ctx);
      if (!arg_type) {
        return hir::kInvalidTaskId;
      }
      SymbolId arg_sym =
          registrar.Register(*arg, SymbolKind::kParameter, arg_type);
      parameters.push_back(arg_sym);
    }

    body_result = LowerStatement(task.getBody(), registrar, ctx);
  }

  hir::StatementId body;
  if (!body_result.has_value()) {
    // Empty body - create empty block
    body = ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kBlock,
            .span = span,
            .data = hir::BlockStatementData{.statements = {}},
        });
  } else if (!*body_result) {
    return hir::kInvalidTaskId;
  } else {
    body = *body_result;
  }

  return ctx->hir_arena->AddTask(
      hir::Task{
          .symbol = symbol,
          .span = span,
          .parameters = std::move(parameters),
          .body = body,
      });
}

}  // namespace lyra::lowering::ast_to_hir
