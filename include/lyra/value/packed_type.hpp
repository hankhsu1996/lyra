#pragma once

#include <cstdint>
#include <span>

#include "lyra/base/fixed_array.hpp"

namespace lyra::value {

// Declared range of one packed dimension. Outermost dimension is dims[0]; the
// inner element type's dim stack follows. For `bit [N-1:0]`: dims = [{N-1,
// 0}] (1D, each element is one bit). For `bit [1:0][7:0]`: dims = [{1, 0},
// {7, 0}] (2D, each outer element is `bit [7:0]`).
//
// This carries SystemVerilog's declared bounds, which are what turns a
// position written in the source into the run of bits it names. Where a value's
// bits live is a separate question and this states nothing about it.
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
// operand an access into a value takes, and the base type an enum declares.
// A shape reaches the runtime as ordinary data -- a dimension stack passed as a
// span, plus the two flags -- so a shape settled while compiling and one
// computed while running are the same construction. An `std::initializer_list`
// parameter, the shorter-looking spelling, could only be the first of those: a
// list is written into the source and nothing builds one from computed dims.
//
// No declaration produces an empty dimension stack, and one is not a valid
// declared type. It answers a width of zero rather than the one an empty
// product would otherwise give, so a stack computed at run time that comes back
// empty does not read as a single bit.
struct PackedType {
  // Almost every declared integral is one-dimensional, so the stack is held
  // inline for that case rather than on the heap.
  using Dims = base::FixedArray<PackedRange, 1>;

  PackedType(
      std::span<const PackedRange> dims, bool is_signed, bool is_four_state)
      : dims(dims.begin(), dims.end()),
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

  Dims dims;
  bool is_signed;
  bool is_four_state;
  std::uint64_t bit_width;
};

}  // namespace lyra::value
