#pragma once

// DPI-C ABI lowering layer for LLVM codegen.
// Owns all foreign ABI decisions: type mapping, function declaration,
// argument marshaling, return coercion, calling convention, and export
// wrapper emission.
// Separate from internal Lyra function ABI; never reuses BuildUserFunctionType.

#include <string>
#include <unordered_map>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/dpi_types.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/statement.hpp"

namespace llvm {
class Function;
class LLVMContext;
class Type;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {
class Context;
}  // namespace lyra::lowering::mir_to_llvm

namespace lyra::lowering::mir_to_llvm::dpi {

// Map a scalar DPI ABI type class to the LLVM type used at the C ABI boundary.
// Handles scalar/direct classes only. Throws for packed-vector ABI classes --
// use GetLlvmDpiStorageType instead.
auto GetLlvmScalarDpiType(llvm::LLVMContext& ctx, DpiAbiTypeClass t)
    -> llvm::Type*;

// Get the LLVM in-memory storage type for a DPI ABI class.
// Packed-vector classes return multiword array types:
//   4-state vectors -> [N x {i32, i32}]  (svLogicVecVal)
//   2-state wide    -> [N x i32]          (svBitVecVal)
// Scalar/direct classes delegate to GetLlvmScalarDpiType.
auto GetLlvmDpiStorageType(
    llvm::LLVMContext& ctx, DpiAbiTypeClass abi_type, TypeId sv_type,
    const TypeArena& types) -> llvm::Type*;

// Declare (or return cached) an external DPI import function with C calling
// convention. Validates signature consistency on cache hit.
auto GetOrDeclareDpiImport(
    Context& context, const std::string& c_name, const mir::DpiSignature& sig)
    -> llvm::Function*;

// Lower a complete DPI import call: marshal and coerce arguments to C ABI
// types, emit call with C calling convention, coerce return value back to
// internal representation, and handle return staging.
auto LowerDpiImportCall(Context& context, const mir::DpiCall& call)
    -> Result<void>;

// Emit public C-ABI wrapper functions for DPI exports.
// Package-scoped exports: direct call to design-global callable.
// Module-scoped exports: instance binding via LyraResolveModuleInstanceBinding
// + direct call to compile-time-known module callable.
// Supports all DPI parameter types: scalar by-value (2-state and 4-state),
// and by-pointer params (packed vectors and scalar output/inout) through
// staged decode/writeback. Packed-vector returns rejected upstream (D4c).
// Must be called after all internal functions are declared and defined
// (both package Phase 3 and module Phase 4).
// Resolved module export callee with contract info for DPI validation.
struct ModuleExportCalleeInfo {
  llvm::Function* llvm_func = nullptr;
  bool accepts_process_ownership = false;
};

auto EmitDpiExportWrappers(
    Context& context, const std::vector<mir::DpiExportWrapperDesc>& exports,
    const std::unordered_map<
        mir::ModuleExportCalleeKey, ModuleExportCalleeInfo,
        mir::ModuleExportCalleeKeyHash>& module_export_callees) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm::dpi
