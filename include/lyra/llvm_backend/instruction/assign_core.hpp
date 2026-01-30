#pragma once

#include <algorithm>
#include <string>
#include <variant>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

// Helper to convert LLVM type to string for error messages.
inline auto LlvmTypeToString(llvm::Type* ty) -> std::string {
  std::string s;
  llvm::raw_string_ostream os(s);
  ty->print(os);
  return s;
}

// Check if LLVM type is the canonical {iN, iN} 4-state scalar struct.
inline auto IsFourStateScalarStruct(llvm::Type* ty) -> bool {
  auto* st = llvm::dyn_cast<llvm::StructType>(ty);
  if (st == nullptr || st->getNumElements() != 2) return false;
  auto* e0 = st->getElementType(0);
  auto* e1 = st->getElementType(1);
  return e0->isIntegerTy() && e1->isIntegerTy() && e0 == e1;
}

// Check if place has any IndexProjection.
inline auto HasIndexProjection(const mir::Place& place) -> bool {
  return std::ranges::any_of(place.projections, [](const auto& proj) {
    return std::holds_alternative<mir::IndexProjection>(proj.info);
  });
}

// Evaluate RightHandSide to a raw LLVM value.
// Handles both Operand and Rvalue sources. For 4-state packed types,
// packs the result into the canonical {val, unk} struct.
auto LowerRhsRaw(
    Context& context, const mir::RightHandSide& rhs, mir::PlaceId target)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm::detail
