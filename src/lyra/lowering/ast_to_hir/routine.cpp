#include "lyra/lowering/ast_to_hir/routine.hpp"

#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/dpi.hpp"
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

auto ConvertParameterDirection(slang::ast::ArgumentDirection dir)
    -> ParameterDirection {
  switch (dir) {
    case slang::ast::ArgumentDirection::In:
      return ParameterDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return ParameterDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return ParameterDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      return ParameterDirection::kRef;
  }
  throw common::InternalError("routine lowering", "unknown argument direction");
}

auto MakeUnsupportedDpiArgDiagnostic(const slang::ast::Type& type)
    -> std::string {
  return std::format(
      "DPI-C argument type '{}' not yet supported", type.toString());
}

auto MakeUnclassifiableDpiReturnDiagnostic(const slang::ast::Type& type)
    -> std::string {
  return std::format(
      "DPI-C return type '{}' not yet supported", type.toString());
}

auto MakeUnsupportedIndirectDpiReturnDiagnostic(
    const slang::ast::Type& type, hir::DpiAbiTypeClass abi_type)
    -> std::string {
  switch (abi_type) {
    case hir::DpiAbiTypeClass::kLogicVecNarrow:
    case hir::DpiAbiTypeClass::kLogicVecWide:
    case hir::DpiAbiTypeClass::kBitVecWide:
      return std::format(
          "DPI-C return type '{}' requires indirect return modeling, "
          "not yet supported",
          type.toString());
    default:
      throw common::InternalError(
          "MakeUnsupportedIndirectDpiReturnDiagnostic",
          std::format(
              "expected indirect-return ABI class, got {}",
              static_cast<int>(abi_type)));
  }
}

}  // namespace

// Canonical DPI ABI type classifier over slang types.
// Shared between import and export classification paths.
auto ClassifyDpiAbiType(const slang::ast::Type& type)
    -> std::optional<hir::DpiAbiTypeClass> {
  const auto& canonical = type.getCanonicalType();

  if (canonical.isVoid()) {
    return hir::DpiAbiTypeClass::kVoid;
  }

  if (canonical.isFloating()) {
    const auto& ft = canonical.as<slang::ast::FloatingType>();
    switch (ft.floatKind) {
      case slang::ast::FloatingType::Real:
        return hir::DpiAbiTypeClass::kReal;
      case slang::ast::FloatingType::RealTime:
        return std::nullopt;
      case slang::ast::FloatingType::ShortReal:
        return hir::DpiAbiTypeClass::kShortReal;
    }
  }

  if (canonical.isString()) {
    return hir::DpiAbiTypeClass::kString;
  }

  if (canonical.isCHandle()) {
    return hir::DpiAbiTypeClass::kChandle;
  }

  // Predefined integer types: byte, shortint, int, longint (2-state);
  // integer (4-state, 32-bit), time (4-state, 64-bit).
  if (canonical.kind == slang::ast::SymbolKind::PredefinedIntegerType) {
    const auto& pit = canonical.as<slang::ast::PredefinedIntegerType>();
    if (!pit.isFourState) {
      switch (pit.integerKind) {
        case slang::ast::PredefinedIntegerType::Byte:
          return hir::DpiAbiTypeClass::kByte;
        case slang::ast::PredefinedIntegerType::ShortInt:
          return hir::DpiAbiTypeClass::kShortInt;
        case slang::ast::PredefinedIntegerType::Int:
          return hir::DpiAbiTypeClass::kInt;
        case slang::ast::PredefinedIntegerType::LongInt:
          return hir::DpiAbiTypeClass::kLongInt;
        default:
          return std::nullopt;
      }
    }
    // 4-state predefined: integer (32-bit) -> kLogicVecNarrow.
    // time (64-bit) is uncommon in DPI; reject for now.
    if (pit.integerKind == slang::ast::PredefinedIntegerType::Integer) {
      return hir::DpiAbiTypeClass::kLogicVecNarrow;
    }
    return std::nullopt;
  }

  // Scalar types: bit (2-state), logic/reg (4-state).
  if (canonical.kind == slang::ast::SymbolKind::ScalarType) {
    const auto& st = canonical.as<slang::ast::ScalarType>();
    if (st.scalarKind == slang::ast::ScalarType::Bit) {
      return hir::DpiAbiTypeClass::kBit;
    }
    if (st.scalarKind == slang::ast::ScalarType::Logic ||
        st.scalarKind == slang::ast::ScalarType::Reg) {
      return hir::DpiAbiTypeClass::kLogicScalar;
    }
    return std::nullopt;
  }

  // Packed integral types: arrays, structs, unions.
  // Split by 2-state vs 4-state, then by width.
  if (canonical.isIntegral()) {
    auto width = canonical.getBitWidth();
    if (!canonical.isFourState()) {
      if (width == 1) return hir::DpiAbiTypeClass::kBit;
      if (width <= 32) return hir::DpiAbiTypeClass::kInt;
      if (width <= 64) return hir::DpiAbiTypeClass::kLongInt;
      return hir::DpiAbiTypeClass::kBitVecWide;
    }
    if (width == 1) return hir::DpiAbiTypeClass::kLogicScalar;
    if (width <= 64) return hir::DpiAbiTypeClass::kLogicVecNarrow;
    return hir::DpiAbiTypeClass::kLogicVecWide;
  }

  return std::nullopt;
}

auto TryLowerDpiImport(
    const slang::ast::SubroutineSymbol& sub, SymbolRegistrar& registrar,
    Context* ctx) -> DpiLoweringResult {
  if (!sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
    return DpiLoweringResult::NotDpi();
  }

  SourceSpan span = ctx->SpanOf(GetSourceRange(sub));

  // Reject DPI tasks.
  if (sub.subroutineKind == slang::ast::SubroutineKind::Task) {
    ctx->sink->Unsupported(
        span, "DPI-C import tasks not yet supported",
        UnsupportedCategory::kFeature);
    return DpiLoweringResult::Rejected();
  }

  // Reject context imports.
  if (sub.flags.has(slang::ast::MethodFlags::DPIContext)) {
    ctx->sink->Unsupported(
        span, "DPI-C context import functions not yet supported",
        UnsupportedCategory::kFeature);
    return DpiLoweringResult::Rejected();
  }

  // Classify return type.
  const auto& ret_type = sub.getReturnType();
  auto return_class = ClassifyDpiAbiType(ret_type);
  if (!return_class) {
    ctx->sink->Unsupported(
        span, MakeUnclassifiableDpiReturnDiagnostic(ret_type),
        UnsupportedCategory::kType);
    return DpiLoweringResult::Rejected();
  }
  if (!hir::IsValidDpiReturnType(*return_class)) {
    ctx->sink->Unsupported(
        span,
        MakeUnsupportedIndirectDpiReturnDiagnostic(ret_type, *return_class),
        UnsupportedCategory::kType);
    return DpiLoweringResult::Rejected();
  }

  TypeId return_type_id = LowerType(ret_type, span, ctx);
  if (!return_type_id) {
    return DpiLoweringResult::Rejected();
  }

  // Phase 1: Validate and classify all parameters without registering symbols.
  // Registration happens only after the full declaration is validated.
  struct PendingParam {
    const slang::ast::FormalArgumentSymbol* arg;
    SourceSpan span;
    TypeId type_id;
    hir::DpiAbiTypeClass dpi_type;
    ParameterDirection direction;
  };
  std::vector<PendingParam> pending_params;
  bool has_error = false;

  for (const slang::ast::FormalArgumentSymbol* arg : sub.getArguments()) {
    SourceSpan arg_span = ctx->SpanOf(GetSourceRange(*arg));

    // Internal consistency: slang rejects ref on DPI subroutines upstream,
    // so this should never be reached. Guard against future slang changes.
    if (arg->direction == slang::ast::ArgumentDirection::Ref) {
      throw common::InternalError(
          "TryLowerDpiImport",
          "ref direction on DPI parameter should have been rejected by slang");
    }

    auto arg_class = ClassifyDpiAbiType(arg->getType());
    if (!arg_class || !hir::IsValidDpiParamType(*arg_class)) {
      ctx->sink->Unsupported(
          arg_span, MakeUnsupportedDpiArgDiagnostic(arg->getType()),
          UnsupportedCategory::kType);
      has_error = true;
      continue;
    }

    TypeId arg_type_id = LowerType(arg->getType(), arg_span, ctx);
    if (!arg_type_id) {
      has_error = true;
      continue;
    }

    pending_params.push_back({
        .arg = arg,
        .span = arg_span,
        .type_id = arg_type_id,
        .dpi_type = *arg_class,
        .direction = ConvertParameterDirection(arg->direction),
    });
  }

  if (has_error) {
    return DpiLoweringResult::Rejected();
  }

  // Phase 2: Declaration is fully validated. Register symbols and build decl.

  // Resolve C function name. Slang does not expose the DPI c_identifier on
  // SubroutineSymbol at the AST level -- only in the syntax tree. This is the
  // only place in the pipeline that touches syntax for DPI; all downstream
  // layers use the resolved c_name from this struct.
  std::string sv_name(sub.name);
  std::string c_name = sv_name;
  const auto* syntax = sub.getSyntax();
  if (syntax != nullptr &&
      syntax->kind == slang::syntax::SyntaxKind::DPIImport) {
    const auto& dpi_syntax = syntax->as<slang::syntax::DPIImportSyntax>();
    auto c_id = dpi_syntax.c_identifier.valueText();
    if (!c_id.empty()) {
      c_name = std::string(c_id);
    }
  }

  // TODO(DPI): introduce a dedicated symbol/declaration kind once design-level
  // DPI registry and call resolution are added; D1 Step 2 reuses kFunction
  // only to keep name binding alive until then.
  SymbolId symbol =
      registrar.Register(sub, SymbolKind::kFunction, return_type_id);

  std::vector<hir::DpiParam> params;
  params.reserve(pending_params.size());
  for (const auto& pp : pending_params) {
    SymbolId arg_sym = registrar.Register(
        *pp.arg, SymbolKind::kParameter, pp.type_id,
        StorageClass::kLocalStorage);
    params.push_back({
        .symbol = arg_sym,
        .span = pp.span,
        .type_id = pp.type_id,
        .dpi_type = pp.dpi_type,
        .direction = pp.direction,
    });
  }

  return DpiLoweringResult::Accepted(
      hir::DpiImportDecl{
          .symbol = symbol,
          .span = span,
          .sv_name = std::move(sv_name),
          .c_name = std::move(c_name),
          .return_type_id = return_type_id,
          .return_dpi_type = *return_class,
          .params = std::move(params),
      });
}

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

  // Lower return type (all types now supported)
  const auto& ret_type = func.getReturnType();
  TypeId return_type = LowerType(ret_type, span, ctx);
  if (!return_type) {
    return hir::kInvalidFunctionId;
  }

  // Check for unsupported parameter directions (ref not yet supported)
  for (const slang::ast::FormalArgumentSymbol* arg : func.getArguments()) {
    if (arg->direction == slang::ast::ArgumentDirection::Ref) {
      ctx->sink->Error(span, "ref parameters not yet supported");
      return hir::kInvalidFunctionId;
    }
  }

  // Register symbol if not already pre-registered
  if (!symbol) {
    symbol = registrar.Register(func, SymbolKind::kFunction, return_type);
  }

  std::vector<hir::FunctionParam> parameters;
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
      ParameterDirection dir = ConvertParameterDirection(arg->direction);
      parameters.push_back({.symbol = arg_sym, .direction = dir});
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

  // Check for unsupported parameter directions (ref not yet supported)
  for (const slang::ast::FormalArgumentSymbol* arg : task.getArguments()) {
    if (arg->direction == slang::ast::ArgumentDirection::Ref) {
      ctx->sink->Error(span, "ref parameters not yet supported");
      return hir::kInvalidTaskId;
    }
  }

  SymbolId symbol = registrar.Register(task, SymbolKind::kTask, kInvalidTypeId);

  std::vector<hir::FunctionParam> parameters;
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
      ParameterDirection dir = ConvertParameterDirection(arg->direction);
      parameters.push_back({.symbol = arg_sym, .direction = dir});
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
