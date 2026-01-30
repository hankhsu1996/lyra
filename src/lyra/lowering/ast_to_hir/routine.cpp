#include "lyra/lowering/ast_to_hir/routine.hpp"

#include <format>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
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
    const slang::ast::ProceduralBlockSymbol& proc, ScopeLowerer& lowerer)
    -> hir::ProcessId {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();

  SourceSpan span = ctx->SpanOf(GetSourceRange(proc));
  hir::ProcessKind kind = ConvertProcessKind(proc.procedureKind);

  std::optional<hir::StatementId> body_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kBlock);
    body_result = LowerStatement(proc.getBody(), lowerer);
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
    const slang::ast::SubroutineSymbol& func, ScopeLowerer& lowerer)
    -> hir::FunctionId {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();

  SourceSpan span = ctx->SpanOf(GetSourceRange(func));

  // Check if symbol was pre-registered as unsupported (two-phase lowering).
  // If so, skip lowering - the error was already emitted at registration.
  SymbolId symbol = registrar.Lookup(func);
  if (symbol) {
    const Symbol& sym = (*ctx->symbol_table)[symbol];
    if (sym.unsupported_reason.has_value()) {
      return hir::kInvalidFunctionId;
    }
  }

  // Check for DPI-C imports (not supported)
  if (func.flags.has(slang::ast::MethodFlags::DPIImport) ||
      func.flags.has(slang::ast::MethodFlags::DPIContext)) {
    ctx->sink->Error(
        span,
        std::format("DPI-C import function '{}' not supported", func.name));
    return hir::kInvalidFunctionId;
  }

  // Reject unsupported return types (for functions not pre-registered)
  const auto& ret_type = func.getReturnType();
  if (!ret_type.isIntegral() && !ret_type.isVoid() && !ret_type.isString()) {
    ctx->sink->Error(
        span, "only integral, void, or string return types supported");
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

  // Register symbol if not already pre-registered
  if (!symbol) {
    symbol = registrar.Register(func, SymbolKind::kFunction, return_type);
  }

  std::vector<SymbolId> parameters;
  std::optional<SymbolId> return_var;
  std::optional<hir::StatementId> body_result;
  {
    ScopeGuard scope_guard(registrar, ScopeKind::kFunction);

    // Register return variable for non-void functions BEFORE lowering body
    // so name lookup works for return-by-name assignment (e.g., `foo = 1;`).
    // slang provides returnValVar for non-void functions.
    if (!ret_type.isVoid() && func.returnValVar != nullptr) {
      return_var = registrar.Register(
          *func.returnValVar, SymbolKind::kVariable, return_type,
          StorageClass::kLocalStorage);
    }

    for (const slang::ast::FormalArgumentSymbol* arg : func.getArguments()) {
      TypeId arg_type = LowerType(arg->getType(), span, ctx);
      if (!arg_type) {
        return hir::kInvalidFunctionId;
      }
      SymbolId arg_sym = registrar.Register(
          *arg, SymbolKind::kParameter, arg_type, StorageClass::kLocalStorage);
      parameters.push_back(arg_sym);
    }

    body_result = LowerStatement(func.getBody(), lowerer);
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
          .return_var = return_var,
      });
}

auto LowerTask(const slang::ast::SubroutineSymbol& task, ScopeLowerer& lowerer)
    -> hir::TaskId {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();

  SourceSpan span = ctx->SpanOf(GetSourceRange(task));

  // Check for DPI-C imports (not supported)
  if (task.flags.has(slang::ast::MethodFlags::DPIImport) ||
      task.flags.has(slang::ast::MethodFlags::DPIContext)) {
    ctx->sink->Error(
        span, std::format("DPI-C import task '{}' not supported", task.name));
    return hir::kInvalidTaskId;
  }

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
      SymbolId arg_sym = registrar.Register(
          *arg, SymbolKind::kParameter, arg_type, StorageClass::kLocalStorage);
      parameters.push_back(arg_sym);
    }

    body_result = LowerStatement(task.getBody(), lowerer);
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
