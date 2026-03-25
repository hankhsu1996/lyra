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

// Map a D1 DPI ABI type class to the LLVM type used at the C ABI boundary.
auto GetLlvmDpiType(llvm::LLVMContext& ctx, DpiAbiTypeClass t) -> llvm::Type*;

// Declare (or return cached) an external DPI import function with C calling
// convention. Validates signature consistency on cache hit.
auto GetOrDeclareDpiImport(
    Context& context, const std::string& c_name, const mir::DpiSignature& sig)
    -> llvm::Function*;

// Lower a complete DPI import call: marshal and coerce arguments to C ABI
// types, emit call with C calling convention, coerce return value back to
// internal representation, and handle return staging.
auto LowerDpiImportCall(
    Context& context, const mir::Call& call, const mir::DpiImportRef& ref)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm::dpi
