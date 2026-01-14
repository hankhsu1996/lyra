#pragma once

#include <memory>
#include <span>

namespace lyra::lir {
struct Function;
struct Module;
}  // namespace lyra::lir

namespace lyra::lowering::mir_to_lir {

/// Resolves function call references in all modules.
///
/// For each kCall instruction:
/// - If name contains "::", look up in package_functions
/// - Otherwise, look up in the instruction's owning module
/// - Stores resolved pointer in instr.callee
///
/// Must be called after LowerModules and LowerPackages.
/// Throws InternalError if a function cannot be resolved.
void LinkFunctionCalls(
    std::span<const std::unique_ptr<lir::Module>> modules,
    std::span<const std::unique_ptr<lir::Function>> package_functions);

/// Resolves submodule instance references to module pointers.
///
/// For each SubmoduleInstance in each module:
/// - Looks up module by signature
/// - Stores resolved pointer in submod.child_module
///
/// Must be called after LowerModules.
/// Throws InternalError if a module cannot be resolved.
void LinkSubmodules(std::span<const std::unique_ptr<lir::Module>> modules);

}  // namespace lyra::lowering::mir_to_lir
