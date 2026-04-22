#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Value.h>

namespace lyra::lowering::mir_to_llvm {

// Semantic bit width of a packed type. Logical width before any storage
// rounding. Not an LLVM type width -- just a count of meaningful bits.
class SemanticBits {
 public:
  static auto FromRaw(uint32_t bits) -> SemanticBits {
    return SemanticBits(bits);
  }

  [[nodiscard]] auto Raw() const -> uint32_t {
    return bits_;
  }

  friend auto operator==(SemanticBits lhs, SemanticBits rhs) -> bool = default;

 private:
  explicit SemanticBits(uint32_t bits) : bits_(bits) {
  }
  uint32_t bits_ = 0;
};

// Canonical storage lane width in bits. Derived from GetStorageByteSize.
// This is the width at which all packed-storage algebra (mask, shift, RMW)
// operates. Always >= SemanticBits and always a multiple of 8 for widths > 64.
// Construction validates non-zero.
class LaneBits {
 public:
  static auto FromRaw(uint32_t bits) -> LaneBits;

  [[nodiscard]] auto Raw() const -> uint32_t {
    return bits_;
  }

  friend auto operator==(LaneBits lhs, LaneBits rhs) -> bool = default;

 private:
  explicit LaneBits(uint32_t bits) : bits_(bits) {
  }
  uint32_t bits_ = 0;
};

// Backing-domain width in bits. The exact LLVM integer width used for
// local allocas and SSA temporaries. For widths <= 64, this is power-of-2
// rounded (same as lane width). For widths > 64, this is the exact semantic
// width (may differ from lane width which is byte-rounded).
class BackingBits {
 public:
  static auto FromRaw(uint32_t bits) -> BackingBits;

  [[nodiscard]] auto Raw() const -> uint32_t {
    return bits_;
  }

  friend auto operator==(BackingBits lhs, BackingBits rhs) -> bool = default;

 private:
  explicit BackingBits(uint32_t bits) : bits_(bits) {
  }
  uint32_t bits_ = 0;
};

// LLVM integer value known to be at canonical lane width.
// Construction goes through FromNormalized only, which validates that the
// LLVM type width matches the declared LaneBits. Do not construct directly.
class LaneValue {
 public:
  static auto FromNormalized(llvm::Value* raw, LaneBits lane_bits) -> LaneValue;

  [[nodiscard]] auto Raw() const -> llvm::Value* {
    return raw_;
  }
  [[nodiscard]] auto Bits() const -> LaneBits {
    return lane_bits_;
  }

 private:
  LaneValue(llvm::Value* raw, LaneBits lane_bits)
      : raw_(raw), lane_bits_(lane_bits) {
  }

  llvm::Value* raw_;
  LaneBits lane_bits_;
};

// Two-plane storage form at canonical lane width.
// val is always present. unk is present only for 4-state storage.
// When unk is present, both planes carry identical LaneBits.
// Used as the return type of LoadLanePlanes and the input type of
// StoreLanePlanes -- the single boundary between backing representation
// and lane-domain code.
class LanePlaneValues {
 public:
  static auto Make(LaneValue val, std::optional<LaneValue> unk)
      -> LanePlaneValues;

  [[nodiscard]] auto Val() const -> const LaneValue& {
    return val_;
  }
  [[nodiscard]] auto Unk() const -> const std::optional<LaneValue>& {
    return unk_;
  }

 private:
  LanePlaneValues(LaneValue val, std::optional<LaneValue> unk)
      : val_(val), unk_(std::move(unk)) {
  }

  LaneValue val_;
  std::optional<LaneValue> unk_;
};

}  // namespace lyra::lowering::mir_to_llvm
