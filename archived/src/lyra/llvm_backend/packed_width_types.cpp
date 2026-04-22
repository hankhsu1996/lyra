#include "lyra/llvm_backend/packed_width_types.hpp"

#include <format>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

auto BackingBits::FromRaw(uint32_t bits) -> BackingBits {
  if (bits == 0) {
    throw common::InternalError(
        "BackingBits::FromRaw", "backing bits must be > 0");
  }
  return BackingBits(bits);
}

auto LaneBits::FromRaw(uint32_t bits) -> LaneBits {
  if (bits == 0) {
    throw common::InternalError("LaneBits::FromRaw", "lane bits must be > 0");
  }
  return LaneBits(bits);
}

auto LaneValue::FromNormalized(llvm::Value* raw, LaneBits lane_bits)
    -> LaneValue {
  if (raw == nullptr) {
    throw common::InternalError(
        "LaneValue::FromNormalized", "raw value must be non-null");
  }
  auto* int_ty = llvm::dyn_cast<llvm::IntegerType>(raw->getType());
  if (int_ty == nullptr || int_ty->getBitWidth() != lane_bits.Raw()) {
    throw common::InternalError(
        "LaneValue::FromNormalized",
        std::format(
            "LLVM type width does not match LaneBits: expected i{}, got {}",
            lane_bits.Raw(),
            raw->getType()->isIntegerTy()
                ? std::format(
                      "i{}", llvm::cast<llvm::IntegerType>(raw->getType())
                                 ->getBitWidth())
                : "non-integer"));
  }
  return LaneValue(raw, lane_bits);
}

auto LanePlaneValues::Make(LaneValue val, std::optional<LaneValue> unk)
    -> LanePlaneValues {
  if (unk.has_value() && unk->Bits() != val.Bits()) {
    throw common::InternalError(
        "LanePlaneValues::Make",
        std::format(
            "val and unk lane bits must match: val={}, unk={}",
            val.Bits().Raw(), unk->Bits().Raw()));
  }
  return LanePlaneValues(val, std::move(unk));
}

}  // namespace lyra::lowering::mir_to_llvm
