#pragma once

#include <cstdint>
#include <span>
#include <utility>
#include <vector>

namespace lyra::value {

// Declared range of one packed dimension. Outermost dimension is dims[0]; the
// inner element type's dim stack follows. For `bit [N-1:0]`: dims = [{N-1,
// 0}] (1D, each element is one bit). For `bit [1:0][7:0]`: dims = [{1, 0},
// {7, 0}] (2D, each outer element is `bit [7:0]`).
//
// This carries SystemVerilog's declared bounds. Storage layout is private --
// PackedArray today uses flat-bit BitValue/LogicValue, but the API contract
// is element-level (operator[] / Slice take outer-element positions), so
// future storage optimizations (canonical byte alignment, vector-of-elements
// for huge outer dims, etc.) change runtime internals without touching the
// emit or this contract.
struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t {
    return static_cast<std::uint64_t>(
        (left >= right ? left - right : right - left) + 1);
  }

  auto operator==(const PackedRange&) const -> bool = default;
};

// The declared type of an integral value: the dimension stack, signedness, and
// state domain, with the total bit width derived from the dimensions once at
// construction. This is the single packed-type descriptor used everywhere a
// shape must be named -- the argument to every PackedArray construction, the
// type each PackedArray owns, and the base type an enum class declares.
// Generated code constructs one inline (`PackedType{{{3,0},{7,0}}, false,
// true}`); a selector builds one from runtime-computed dims; either way the
// shape reaches the runtime as ordinary data, never an `std::initializer_list`
// parameter.
//
// An empty dimension stack is the bit-width-0 form an as-yet-uninitialized cell
// holds before its first store installs a representation; it is not a valid
// declared type.
struct PackedType {
  PackedType(std::vector<PackedRange> dims, bool is_signed, bool is_four_state)
      : dims(std::move(dims)),
        is_signed(is_signed),
        is_four_state(is_four_state),
        bit_width(WidthOf(this->dims)) {
  }

  // Total bit count of a dimension stack: the product of every dimension's
  // element count, 0 for the empty stack. The single source for width-from-dims
  // -- the constructor derives `bit_width` through it, and every runtime caller
  // that needs the width of a freshly computed dim stack routes here.
  [[nodiscard]] static auto WidthOf(std::span<const PackedRange> dims)
      -> std::uint64_t {
    std::uint64_t width = 1;
    for (const auto& range : dims) {
      width *= range.ElementCount();
    }
    return dims.empty() ? 0U : width;
  }

  // Type equality across every axis a stored value must match: the dimension
  // stack, signedness, and state domain. Bit width is derived from the
  // dimensions, so it never needs comparing independently.
  [[nodiscard]] auto SameRepresentation(const PackedType& other) const -> bool {
    return dims == other.dims && is_signed == other.is_signed &&
           is_four_state == other.is_four_state;
  }

  std::vector<PackedRange> dims;
  bool is_signed;
  bool is_four_state;
  std::uint64_t bit_width;
};

}  // namespace lyra::value
