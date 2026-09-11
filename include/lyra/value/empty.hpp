#pragma once

#include <string>

#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// A value carrying no information: it has exactly one value, so each value
// operation is the constant that follows from having nothing to compare or
// scan. SystemVerilog reaches it through a tagged union's `void` member (LRM
// 7.3.2). Being an ordinary value type is what keeps every generic operation
// over a tagged union's components uniform -- it answers the same questions as
// any other component, so nothing that folds over them has to know it exists.
//
// The comparison operands are unnamed because the value concept's signatures
// mandate them while a type with one value has nothing to read from them.
struct Empty {
  // NOLINTNEXTLINE(readability-named-parameter)
  auto operator==(const Empty&) const -> PackedArray {
    return PackedArray::Bit(true);
  }
  auto operator!=(const Empty& other) const -> PackedArray {
    return !(*this == other);
  }
  // NOLINTNEXTLINE(readability-named-parameter)
  [[nodiscard]] static auto CaseEqual(const Empty&) -> PackedArray {
    return PackedArray::Bit(true);
  }
  // NOLINTNEXTLINE(readability-named-parameter)
  [[nodiscard]] static auto IsBitIdentical(const Empty&) -> bool {
    return true;
  }
  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }
  [[nodiscard]] static auto IsUnknown() -> PackedArray {
    return PackedArray::Bit(false);
  }
  // LRM 20.6.2 `$bits` / 20.9 `$countbits`: a value carrying no bits.
  [[nodiscard]] static auto BitstreamWidth() -> PackedArray {
    return PackedArray::Int(0);
  }
  // NOLINTNEXTLINE(readability-named-parameter)
  [[nodiscard]] static auto CountBits(const PackedArray&) -> PackedArray {
    return PackedArray::Int(0);
  }
};

// A value carrying no bits renders as no text: the tag alone says which member
// a tagged union holds (LRM 7.3.2), so there is nothing beside it to print.
// Answering here is what keeps a fold over a tagged union's components from
// having to know this component exists.
template <>
struct Formatter<Empty> {
  // NOLINTNEXTLINE(readability-named-parameter)
  static auto Format(const FormatSpec&, const Empty&) -> std::string {
    return std::string{};
  }
};

static_assert(LyraValue<Empty>);
static_assert(CaseEqualComparable<Empty>);
static_assert(BitstreamSizable<Empty>);

}  // namespace lyra::value
