#pragma once

#include <optional>
#include <utility>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;
struct Context;
class SymbolRegistrar;

auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& proc, ScopeLowerer& lowerer)
    -> hir::ProcessId;

auto LowerFunction(
    const slang::ast::SubroutineSymbol& func, ScopeLowerer& lowerer)
    -> hir::FunctionId;

auto LowerTask(const slang::ast::SubroutineSymbol& task, ScopeLowerer& lowerer)
    -> hir::TaskId;

// Result of attempting to normalize a DPI import declaration.
enum class DpiLoweringKind {
  kNotDpi,    // Not a DPI import; caller should proceed with normal lowering.
  kAccepted,  // Accepted D1 DPI import; decl is populated.
  kRejected,  // Unsupported DPI form; diagnostics already emitted.
};

struct DpiLoweringResult {
  DpiLoweringKind kind = DpiLoweringKind::kNotDpi;
  std::optional<hir::DpiImportDecl> decl;

  static auto NotDpi() -> DpiLoweringResult {
    return {.kind = DpiLoweringKind::kNotDpi, .decl = std::nullopt};
  }
  static auto Rejected() -> DpiLoweringResult {
    return {.kind = DpiLoweringKind::kRejected, .decl = std::nullopt};
  }
  static auto Accepted(hir::DpiImportDecl d) -> DpiLoweringResult {
    return {.kind = DpiLoweringKind::kAccepted, .decl = std::move(d)};
  }
};

// Try to normalize a subroutine as a DPI import declaration.
// Returns kNotDpi if the subroutine is not a DPI import.
// Returns kAccepted with a populated decl for supported D1 imports.
// Returns kRejected for unsupported DPI forms (diagnostics already emitted).
auto TryLowerDpiImport(
    const slang::ast::SubroutineSymbol& sub, SymbolRegistrar& registrar,
    Context* ctx) -> DpiLoweringResult;

}  // namespace lyra::lowering::ast_to_hir
