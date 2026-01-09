#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <vector>

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
/// @param global_precision Optional precomputed global precision. If not
///        provided, uses the module's own precision (or default).
/// @return The lowered LIR module
auto LowerModule(
    const mir::Module& module,
    std::optional<int8_t> global_precision = std::nullopt)
    -> std::unique_ptr<lir::Module>;

/// Lowers multiple MIR Modules into LIR Modules with shared global precision.
///
/// First computes the global precision as the finest (smallest) precision
/// across all modules, then lowers each module using that shared precision.
/// This ensures consistent delay scaling across module boundaries.
///
/// @param modules The MIR modules to lower
/// @return Vector of lowered LIR modules
auto LowerModules(std::span<const std::unique_ptr<mir::Module>> modules)
    -> std::vector<std::unique_ptr<lir::Module>>;

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
/// This is the package equivalent of LowerModules. It handles all package
/// contents: variable initializers become a single init process, and functions
/// are lowered with qualified names (e.g., "MyPkg::add").
///
/// @param packages The MIR packages to lower
/// @return Lowering result containing init process, functions, and shared
/// context
auto LowerPackages(std::span<const std::unique_ptr<mir::Package>> packages)
    -> PackageLoweringResult;

}  // namespace lyra::lowering::mir_to_lir
