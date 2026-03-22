#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

// Semantic state-domain of a temp value. Distinct from MIR declared type
// and LLVM value shape -- neither of those may proxy for domain.
enum class ValueDomain { kTwoState, kFourState };

// Explicit first-class temp semantic carrier. Stores declared type, semantic
// domain, and split payload planes. Payload convention is uniform:
//   kTwoState:  value = scalar payload, unknown = nullptr
//   kFourState: value = known plane, unknown = unknown plane (both scalar)
// No TempValue ever stores a raw packed {val, unk} struct in value.
struct TempValue {
  TypeId declared_type = {};
  ValueDomain domain = ValueDomain::kTwoState;
  llvm::Value* value = nullptr;
  // nullptr iff domain == kTwoState
  llvm::Value* unknown = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
