#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <vector>

namespace lyra::common {
class SymbolTable;
}

namespace lyra::mir {
class Module;
class Package;
}  // namespace lyra::mir

namespace lyra::lir {
class LirContext;
struct Function;
class Module;
class Process;
}  // namespace lyra::lir

namespace lyra::lowering::mir_to_lir {

/// Lowers a MIR Module into a LIR Module.
///
/// @param module The MIR module to lower
/// @param symbol_table Symbol table for looking up parameter constants
/// @param global_precision Optional precomputed global precision. If not
///        provided, uses the module's own precision (or default).
/// @return The lowered LIR module
auto LowerModule(
    const mir::Module& module, const common::SymbolTable& symbol_table,
    std::optional<int8_t> global_precision = std::nullopt)
    -> std::unique_ptr<lir::Module>;

/// Result of lowering packages to LIR.
struct PackageLoweringResult {
  /// Process that initializes all package variables.
  /// nullptr if no packages have variable initializers.
  std::shared_ptr<lir::Process> init_process;

  /// Functions defined in packages, with qualified names (e.g., "MyPkg::add").
  std::vector<std::unique_ptr<lir::Function>> functions;

  /// Shared LIR context used for lowering.
  std::shared_ptr<lir::LirContext> context;
};

/// Lowers MIR Packages into LIR (init process + functions).
///
/// Handles all package contents: variable initializers become a single init
/// process, and functions are lowered with qualified names (e.g.,
/// "MyPkg::add").
///
/// @param packages The MIR packages to lower
/// @param symbol_table Symbol table for looking up parameter constants
/// @return Lowering result containing init process, functions, and shared
/// context
auto LowerPackages(
    std::span<const std::unique_ptr<mir::Package>> packages,
    const common::SymbolTable& symbol_table) -> PackageLoweringResult;

}  // namespace lyra::lowering::mir_to_lir
