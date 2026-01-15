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

  registrar.PushScope(ScopeKind::kBlock);

  auto body_result = LowerStatement(proc.getBody(), registrar, ctx);

  registrar.PopScope();

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

  TypeId return_type = LowerType(func.getReturnType(), span, ctx);
  if (!return_type) {
    return hir::kInvalidFunctionId;
  }

  SymbolId symbol =
      registrar.Register(func, SymbolKind::kFunction, return_type);

  registrar.PushScope(ScopeKind::kFunction);

  std::vector<SymbolId> parameters;
  for (const slang::ast::FormalArgumentSymbol* arg : func.getArguments()) {
    TypeId arg_type = LowerType(arg->getType(), span, ctx);
    if (!arg_type) {
      registrar.PopScope();
      return hir::kInvalidFunctionId;
    }
    SymbolId arg_sym =
        registrar.Register(*arg, SymbolKind::kParameter, arg_type);
    parameters.push_back(arg_sym);
  }

  auto body_result = LowerStatement(func.getBody(), registrar, ctx);

  registrar.PopScope();

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

  registrar.PushScope(ScopeKind::kTask);

  std::vector<SymbolId> parameters;
  for (const slang::ast::FormalArgumentSymbol* arg : task.getArguments()) {
    TypeId arg_type = LowerType(arg->getType(), span, ctx);
    if (!arg_type) {
      registrar.PopScope();
      return hir::kInvalidTaskId;
    }
    SymbolId arg_sym =
        registrar.Register(*arg, SymbolKind::kParameter, arg_type);
    parameters.push_back(arg_sym);
  }

  auto body_result = LowerStatement(task.getBody(), registrar, ctx);

  registrar.PopScope();

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
