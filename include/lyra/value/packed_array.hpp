#pragma once

#include <cstdint>
#include <initializer_list>
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

  // Convenience factory for the default int shape (32-bit, signed, 2-state).
  // Matches SystemVerilog's default int literal type, so `int x = 5;` lowers
  // to `PackedArray x = PackedArray::Int(5);`.
  [[nodiscard]] static auto Int(std::int32_t value) -> PackedArray;

  // Construct a narrow PackedArray (bit_width <= 64) from an integer value.
  // The `std::int64_t` parameter is the carrier type wide enough to cover
  // every narrow width; the resulting shape is set by `bit_width`, with bits
  // above masked out and the unknown plane left at zero.
  [[nodiscard]] static auto FromInt(
      std::int64_t value, std::uint64_t bit_width, bool is_signed,
      bool is_four_state) -> PackedArray;

  // Construct a PackedArray of arbitrary width from raw word planes. Used
  // by cpp emit for any literal that does not fit a single int64 carrier:
  // widths > 64 bits, or 4-state literals carrying X/Z bits. `value_words`
  // must have ceil(bit_width / 64) entries; bits above `bit_width` in the
  // top word are masked. For 2-state shapes `unknown_words` must be empty;
  // for 4-state shapes it must either be empty (no X/Z) or match
  // `value_words` in size.
  [[nodiscard]] static auto FromWords(
      std::initializer_list<std::uint64_t> value_words,
      std::initializer_list<std::uint64_t> unknown_words,
      std::uint64_t bit_width, bool is_signed, bool is_four_state)
      -> PackedArray;

  // Width-aware conversion. Constructs a fresh PackedArray of the
  // destination shape and copies bits from `src`, sign- or zero-extending
  // per `src`'s signedness when widening, truncating when narrowing.
  // 2-state -> 4-state leaves the unknown plane zero; 4-state -> 2-state
  // collapses any X/Z bits to 0. Both wide (>64-bit) and 4-state paths are
  // supported.
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

  // Variable assignment in SystemVerilog preserves the destination's declared
  // shape; the producer is expected to insert any width/state conversion (the
  // cpp backend lowers slang's ConversionExpr into PackedArray::ConvertFrom).
  // Both copy- and move-assignment route through AssignFrom, which throws
  // InternalError on shape mismatch. This makes "(target = value)" the single
  // emit shape for variable assignment and surfaces any missing conversion as
  // a backend bug rather than silently relayout-ing the destination.
  // Construction (copy/move ctor) does the opposite: a freshly-constructed
  // PackedArray adopts the source's shape, since there is no declared shape
  // to preserve.
  PackedArray(const PackedArray&) = default;
  PackedArray(PackedArray&&) noexcept = default;
  auto operator=(const PackedArray& other) -> PackedArray&;
  auto operator=(PackedArray&& other) noexcept(false) -> PackedArray&;
  ~PackedArray() = default;

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
  // Single source of truth for "construct a PackedArray with a known value".
  // FromInt, FromWords, and any internal op that needs to materialize a
  // result from word planes must delegate here. The helper guarantees both
  // planes are fully written (value from `value_words`, unknown from
  // `unknown_words` or zero when empty); no caller can leak the all-X
  // residue left by LogicValue's default ctor.
  [[nodiscard]] static auto MakeFromWordPlanes(
      std::uint64_t bit_width, bool is_signed, bool is_four_state,
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words) -> PackedArray;

  std::uint64_t bit_width_;
  bool is_signed_;
  bool is_four_state_;
  std::variant<BitValue, LogicValue> storage_;
};

}  // namespace lyra::value
