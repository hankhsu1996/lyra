#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <vector>

namespace lyra::mir {
class Module;
}

namespace lyra::lir {
class Module;
}

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

}  // namespace lyra::lowering::mir_to_lir
