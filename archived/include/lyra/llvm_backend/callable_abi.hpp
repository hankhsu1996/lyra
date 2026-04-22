#pragma once

// Generic internal callable ABI classification.
//
// This is the canonical rule for how Lyra semantic value types map to
// LLVM ABI types at the internal callable boundary (user functions,
// export-wrapper handoff). It is separate from the DPI ABI layer and
// from the layout/storage layer.
//
// The classification is about boundary transport shape only, not about
// runtime ownership or lifetime management.

#include <cstdint>
#include <optional>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

namespace llvm {
class IRBuilderBase;
class Value;
}  // namespace llvm

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Callable value ABI representation class.
// Describes how a direct value is transported at the internal callable
// boundary. This is strictly about ABI representation shape, not about
// runtime ownership or lifetime semantics.
enum class CallableValueReprClass : uint8_t {
  // Pointer-valued: string handles, dynamic containers, chandle.
  // LLVM type: ptr (opaque pointer). Passed and returned directly.
  kPointerValue,
  // IEEE 754 double-precision floating point.
  kReal,
  // IEEE 754 single-precision floating point.
  kShortReal,
  // 2-state packed integer value (bit, byte, int, longint, packed arrays).
  kPackedTwoState,
  // 4-state packed value (logic, integer, packed 4-state arrays).
  // Uses Lyra's two-plane {val, unk} struct representation.
  kPackedFourState,
};

struct CallableValueAbiInfo {
  CallableValueReprClass repr_class;
  llvm::Type* llvm_type;
};

// Classify a Lyra semantic type into its generic internal callable ABI
// representation. This is the single canonical classification for direct
// value passing and returning in user functions.
//
// Returns nullopt for types that cannot be passed by value (aggregates).
// Throws InternalError for void (which must not reach this classifier).
auto ClassifyCallableValueAbi(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> std::optional<CallableValueAbiInfo>;

// Get the LLVM ABI type for a direct value at the internal callable boundary.
// Thin wrapper over ClassifyCallableValueAbi that returns only the LLVM type.
//
// Returns nullptr for aggregate types (caller must handle via out-param).
// Throws InternalError for void or unclassifiable types.
auto GetCallableAbiLlvmType(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type*;

// Form the LLVM call actual for a ref/const-ref formal from a raw void*
// pointer (e.g., from a DeferredAssertionRefBindingAbi::addr). Produces
// the same IR representation that normal call lowering would use for a ref
// formal (pointer to storage). Used by deferred thunk emission now, and
// by general ref-call lowering later.
//
// `kind` must be kRef or kConstRef; asserts on other values.
auto FormRefCallActual(
    llvm::IRBuilderBase& builder, llvm::Value* raw_ptr, mir::PassingKind kind)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
