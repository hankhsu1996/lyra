#pragma once

// DPI-C ABI lowering layer for LLVM codegen.
// Owns all import-side foreign ABI decisions: type mapping, function
// declaration, argument marshaling, return coercion, and calling convention.
// Separate from internal Lyra function ABI; never reuses BuildUserFunctionType.

#include <string>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/dpi_types.hpp"
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
// Only handles D1/D2 scalar classes plus kLogicScalar (svLogic = i8).
// Throws for kLogicVecNarrow -- use GetLlvmDpiStorageType instead.
auto GetLlvmScalarDpiType(llvm::LLVMContext& ctx, DpiAbiTypeClass t)
    -> llvm::Type*;

// Get the LLVM in-memory storage type for a DPI ABI class, accounting for
// 4-state vector types that use svLogicVecVal arrays. Used for staged-temp
// allocation (params) and will extend to return storage (D3b).
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

}  // namespace lyra::lowering::mir_to_llvm::dpi
