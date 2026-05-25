#pragma once

#include <cstdint>
#include <span>
#include <variant>

#include "lyra/value/packed.hpp"

namespace lyra::value {

// Unified integral value. Mirrors slang's `IntegralType` and the legacy
// archive's `IntegralInfo`: one type, three attributes. `backend::cpp` emits
// every SystemVerilog integral (`byte`, `shortint`, `int`, `longint`,
// `integer`, `time`, `bit [N:0]`, `logic [N:0]`, `reg [N:0]`) as
// `PackedArray`. Storage layout (inline word(s) versus heap) and the
// presence of an unknown plane are private details chosen at construction
// from `bit_width` and `is_four_state`.
class PackedArray {
 public:
  PackedArray(std::uint64_t bit_width, bool is_signed, bool is_four_state);

  // Construct a 2-state PackedArray initialised from a 64-bit value. Bits
  // above bit_width are masked out. The unknown plane is zero (no X/Z).
  // For 4-state targets, X/Z would be lost.
  [[nodiscard]] static auto FromInt64(
      std::int64_t value, std::uint64_t bit_width, bool is_signed,
      bool is_four_state) -> PackedArray;

  // Width-aware conversion. Constructs a fresh PackedArray of the
  // destination shape and copies bits from `src`, sign- or zero-extending
  // per `src`'s signedness when widening, truncating when narrowing.
  // Cross-state conversions clear X/Z to 0 when converting 4-state to
  // 2-state. TODO(hankhsu): wide (>64-bit) and 4-state conversion paths
  // throw `InternalError` pending the wide arithmetic helpers.
  [[nodiscard]] static auto ConvertFrom(
      const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
      bool dst_is_four_state) -> PackedArray;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsSigned() const -> bool;
  [[nodiscard]] auto IsFourState() const -> bool;

  // Word-level access for `RuntimeValueView` construction and intra-runtime
  // interop. The spans alias the PackedArray's storage and stay valid for
  // the object's lifetime.
  [[nodiscard]] auto ValueWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto UnknownWords() const -> std::span<const std::uint64_t>;

  // Typed view accessors. The atom encoded in `is_four_state_` selects which
  // overload is callable: 2-state stores expose Bit views, 4-state stores
  // expose Logic views. Calling the wrong one throws InternalError.
  [[nodiscard]] auto AsBitView() -> BitView;
  [[nodiscard]] auto AsBitView() const -> ConstBitView;
  [[nodiscard]] auto AsLogicView() -> LogicView;
  [[nodiscard]] auto AsLogicView() const -> ConstLogicView;

  // Width-aware copy. The destination's attributes drive sign-extension,
  // zero-extension, or truncation as appropriate.
  auto AssignFrom(const PackedArray& other) -> void;

  // Extract the value as a 64-bit signed integer. Sign-extends from
  // bit_width when is_signed_ is true. X/Z bits map to 0. bit_width must
  // be <= 64.
  [[nodiscard]] auto ToInt64() const -> std::int64_t;

  // SystemVerilog "non-zero" interpretation: any non-zero value bit makes
  // the result true. X/Z bits do not count as non-zero. Used by `if`,
  // `while`, and ternary conditions emitted as `if (cond.IsTruthy())`.
  [[nodiscard]] auto IsTruthy() const -> bool;

  // Binary operators. Operands are assumed to share the same
  // `(bit_width, is_signed, is_four_state)` shape (slang's promotion
  // contract). Comparison and logical operators produce a 1-bit 2-state
  // result. Shift operators take the amount as a separate PackedArray;
  // the amount's attributes are self-determined per LRM 11.6.
  [[nodiscard]] auto operator+(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator-(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator*(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator/(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator%(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto operator&(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator|(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator^(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto operator==(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator<(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator<=(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>=(const PackedArray& other) const -> PackedArray;

  // Unary operators.
  [[nodiscard]] auto operator-() const -> PackedArray;
  [[nodiscard]] auto operator~() const -> PackedArray;

  // Methods for ops that have no C++ operator counterpart.
  [[nodiscard]] auto LogicalAnd(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto LogicalOr(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto LogicalNot() const -> PackedArray;

  [[nodiscard]] auto BitwiseXnor(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto ShiftLeft(const PackedArray& amount) const -> PackedArray;
  [[nodiscard]] auto LogicalShiftRight(const PackedArray& amount) const
      -> PackedArray;
  [[nodiscard]] auto ArithmeticShiftRight(const PackedArray& amount) const
      -> PackedArray;

  [[nodiscard]] auto Power(const PackedArray& exponent) const -> PackedArray;

  [[nodiscard]] auto ReductionAnd() const -> PackedArray;
  [[nodiscard]] auto ReductionOr() const -> PackedArray;
  [[nodiscard]] auto ReductionXor() const -> PackedArray;
  [[nodiscard]] auto ReductionNand() const -> PackedArray;
  [[nodiscard]] auto ReductionNor() const -> PackedArray;
  [[nodiscard]] auto ReductionXnor() const -> PackedArray;

 private:
  std::uint64_t bit_width_;
  bool is_signed_;
  bool is_four_state_;
  std::variant<BitValue, LogicValue> storage_;
};

}  // namespace lyra::value
