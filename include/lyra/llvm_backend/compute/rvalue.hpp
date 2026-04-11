#pragma once

#include <cstdint>

#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

enum class PlaceKind {
  kIntegral,
  kManagedHandle,
  kPointerScalar,
};

struct PlaceTypeInfo {
  PlaceKind kind;
  uint32_t bit_width;
  bool is_four_state;
};

// Get type info from PlaceId (validates and extracts TypeId internally).
auto ValidateAndGetTypeInfo(
    const CuFacts& facts, Context& context, mir::PlaceId place_id)
    -> Result<PlaceTypeInfo>;

// Get type info directly from TypeId.
auto GetTypeInfoFromType(const CuFacts& facts, Context& context, TypeId type_id)
    -> Result<PlaceTypeInfo>;

// Get LLVM type for a packed, managed handle, or pointer scalar type.
// Packed: iN (2-state) or {iN, iN} (4-state). Managed/pointer: ptr.
auto GetLlvmTypeForType(const CuFacts& facts, Context& context, TypeId type_id)
    -> Result<llvm::Type*>;

// Get the LLVM scalar element type for the known/unknown planes of a 4-state
// typed value. This is the canonical helper -- do not derive the plane type
// ad-hoc by stripping struct element types. Used by split-PHI creation and
// PHI edge zero synthesis.
auto GetFourStatePlaneType(
    const CuFacts& facts, Context& context, TypeId type_id) -> llvm::Type*;

// Context for lowering packed (integral) compute operations.
// Contains all type information needed by both 2-state and 4-state lowering.
struct PackedComputeContext {
  const CuFacts* facts;      // Read-only type and state facts
  llvm::Type* storage_type;  // The LLVM type for the target place
  llvm::Type* element_type;  // For operations: iN for both 2s and 4s
  uint32_t bit_width;        // Semantic width for masking/shifts
  bool is_four_state;        // Drives the single branch point
};

}  // namespace lyra::lowering::mir_to_llvm
