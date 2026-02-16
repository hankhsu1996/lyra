#pragma once

#include <cstdint>

namespace lyra::runtime {

// Stack-based bytecode for index expressions in dynamic-index edge triggers.
// Used to evaluate compound index expressions (e.g., bus[i+j], bus[i*2+1])
// at runtime from design-state slot values.
//
// Arithmetic semantics: all operations are 64-bit two's complement wraparound.
// kReadSlot masks to bit_width and sign-extends if is_signed, producing a
// well-defined int64. Intermediate results are not truncated.
struct IndexPlanOp {
  enum class Kind : uint8_t {
    kReadSlot = 0,
    kConst = 1,
    kAdd = 2,
    kSub = 3,
    kMul = 4,
    kAnd = 5,
    kOr = 6,
    kXor = 7,
    kShl = 8,
    kLShr = 9,
    kAShr = 10,
    kDivS = 11,
    kDivU = 12,
    kModS = 13,
    kModU = 14,
  };

  Kind kind = Kind::kConst;
  uint8_t bit_width = 0;
  uint8_t is_signed = 0;
  uint8_t byte_size = 0;
  uint32_t slot_id = 0;
  uint32_t byte_offset = 0;
  uint32_t padding = 0;
  int64_t const_value = 0;

  static constexpr auto MakeReadSlot(
      uint32_t slot, uint32_t off, uint8_t bsz, uint8_t bw, bool sign)
      -> IndexPlanOp {
    return {
        .kind = Kind::kReadSlot,
        .bit_width = bw,
        .is_signed = static_cast<uint8_t>(sign ? 1 : 0),
        .byte_size = bsz,
        .slot_id = slot,
        .byte_offset = off};
  }

  static constexpr auto MakeConst(int64_t v) -> IndexPlanOp {
    return {.kind = Kind::kConst, .const_value = v};
  }

  static constexpr auto MakeBinaryOp(Kind k) -> IndexPlanOp {
    return {.kind = k};
  }
};

static_assert(sizeof(IndexPlanOp) == 24);
static_assert(alignof(IndexPlanOp) == 8);

inline constexpr uint32_t kMaxPlanOps = 32;
inline constexpr uint32_t kMaxPlanStackDepth = 32;

}  // namespace lyra::runtime
