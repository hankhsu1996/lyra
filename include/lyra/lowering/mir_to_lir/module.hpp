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

/// Lowers package variable initializers into a synthetic LIR process.
///
/// Creates a process that initializes all package variables. The process
/// should be executed before module elaboration with nullptr instance context
/// to store values in the global variable table.
///
/// @param packages The MIR packages containing variables to initialize
/// @param context Shared LIR context for temp/literal allocation
/// @return The init process, or nullptr if no initializers exist
auto LowerPackageInitProcess(
    std::span<const std::unique_ptr<mir::Package>> packages,
    std::shared_ptr<lir::LirContext> context) -> std::shared_ptr<lir::Process>;

}  // namespace lyra::lowering::mir_to_lir
