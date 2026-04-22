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
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {
class SlotAccessResolver;
}  // namespace lyra::lowering::mir_to_llvm

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
// Resolve destination type from a WriteTarget (PlaceId or ExternalRefId).
auto ResolveDestType(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest)
    -> TypeId;

// Canonical core is TypeId-based; PlaceId overloads derive TypeId and forward.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, TypeId target_type) -> Result<llvm::Value*>;

// PlaceId overloads: derive TypeId from Place and forward.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    mir::PlaceId target) -> Result<llvm::Value*>;
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, mir::PlaceId target) -> Result<llvm::Value*>;

// No-resolver TypeId overload.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    TypeId target_type) -> Result<llvm::Value*>;

// Evaluate RightHandSide to a non-lossy PackedRValue.
// This is the sole transport boundary for packed-store entry points.
// unk == nullptr means the RHS is provably 2-state; no downstream code
// may coerce it to a zero constant.
//
// Operand branch: calls LowerOperandRaw, then extracts from LLVM type
// (struct -> 4-state, scalar -> 2-state). The LLVM type is authoritative
// because it was determined by the operand's own declared type.
// Rvalue branch: calls LowerRvalue and copies value/unknown directly.
auto LowerRhsToPackedRValue(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    uint32_t semantic_bits, TypeId result_type)
    -> Result<lyra::lowering::mir_to_llvm::PackedRValue>;

// Resolver-aware overload.
auto LowerRhsToPackedRValue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, uint32_t semantic_bits, TypeId result_type)
    -> Result<lyra::lowering::mir_to_llvm::PackedRValue>;

}  // namespace lyra::lowering::mir_to_llvm::detail
