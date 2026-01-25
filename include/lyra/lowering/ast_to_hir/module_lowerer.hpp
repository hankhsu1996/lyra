#pragma once

#include <cstdint>

#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

/// Per-scope state for AST→HIR lowering.
/// Immutable after construction; safe for parallel lowering.
struct LoweringFrame {
  int unit_power;  // Scope's timeunit as power of 10 (e.g., -9 for 1ns)
  int global_precision_power;  // Compilation-wide finest precision
};

/// Drives AST→HIR lowering for a single module or package.
/// Each scope gets its own ScopeLowerer; no shared mutable state.
class ScopeLowerer {
 public:
  /// Construct for module instance lowering
  ScopeLowerer(
      Context& ctx, SymbolRegistrar& registrar,
      const slang::ast::InstanceSymbol& instance);

  /// Construct for package lowering
  ScopeLowerer(
      Context& ctx, SymbolRegistrar& registrar,
      const slang::ast::PackageSymbol& package);

  /// Access the compilation-wide context (arenas, diagnostics, etc.)
  [[nodiscard]] auto Ctx() -> Context& {
    return ctx_;
  }
  [[nodiscard]] auto Ctx() const -> const Context& {
    return ctx_;
  }

  /// Access the symbol registrar
  [[nodiscard]] auto Registrar() -> SymbolRegistrar& {
    return registrar_;
  }

  /// Scale a delay literal from scope timeunit to global precision ticks.
  /// Throws InternalError on overflow or invalid exponent.
  [[nodiscard]] auto ScaleDelayTicks(uint64_t literal_ticks) const -> uint64_t;

  /// Get the lowering frame (for testing/debugging)
  [[nodiscard]] auto Frame() const -> const LoweringFrame& {
    return frame_;
  }

 private:
  /// Private constructor with pre-computed frame
  ScopeLowerer(Context& ctx, SymbolRegistrar& registrar, LoweringFrame frame);

  Context& ctx_;
  SymbolRegistrar& registrar_;
  LoweringFrame frame_;
};

// Type alias for backward compatibility
using ModuleLowerer = ScopeLowerer;

}  // namespace lyra::lowering::ast_to_hir
