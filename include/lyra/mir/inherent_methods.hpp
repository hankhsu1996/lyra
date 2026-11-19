#pragma once

#include <array>
#include <cstddef>
#include <span>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// A kind of array's own methods, as a Rust type's inherent impl names its
// items: each by the name the source asks it by. A method the library's traits
// require of a type is an item of that trait, never one of these. Every kind
// whose elements a position names has the methods on a run of its elements:
// the run as bits, the array with the run replaced, its elements reversed, and
// its elements put in an order (LRM 7.4.6, 7.12).
//
// A fixed-size array's (LRM 7.4).
inline constexpr std::array kFixedArrayMethods{
    support::BuiltinFn::kElementSlice,
    support::BuiltinFn::kAssignElementSlice,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
    support::BuiltinFn::kUnpackedArrayFromArray,
};

// A dynamic array's (LRM 7.5).
inline constexpr std::array kDynamicArrayMethods{
    support::BuiltinFn::kDynamicArrayNew,
    support::BuiltinFn::kDynamicArrayNewCopy,
    support::BuiltinFn::kElement,
    support::BuiltinFn::kElementMut,
    support::BuiltinFn::kDelete,
    support::BuiltinFn::kSize,
    support::BuiltinFn::kDynamicArrayFromArray,
    support::BuiltinFn::kElementSlice,
    support::BuiltinFn::kAssignElementSlice,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
};

// A queue's (LRM 7.10).
inline constexpr std::array kQueueMethods{
    support::BuiltinFn::kSize,
    support::BuiltinFn::kElement,
    support::BuiltinFn::kQueueElementMut,
    support::BuiltinFn::kDelete,
    support::BuiltinFn::kDeleteIndex,
    support::BuiltinFn::kInsert,
    support::BuiltinFn::kPushBack,
    support::BuiltinFn::kPushFront,
    support::BuiltinFn::kPopBack,
    support::BuiltinFn::kPopFront,
    support::BuiltinFn::kQueueSlice,
    support::BuiltinFn::kAssignQueueSlice,
    support::BuiltinFn::kQueueAllocate,
    support::BuiltinFn::kQueueHoldToBound,
    support::BuiltinFn::kQueueFromArray,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
};

// Whether `methods` and `rows` name the same methods in the same order, which
// a table building each method of a kind is held to at compile time.
template <typename Row, std::size_t N>
consteval auto NamesTheSameMethods(
    std::span<const support::BuiltinFn> methods, const std::array<Row, N>& rows)
    -> bool {
  if (methods.size() != rows.size()) {
    return false;
  }
  for (std::size_t i = 0; i < rows.size(); ++i) {
    if (methods[i] != rows[i].name) {
      return false;
    }
  }
  return true;
}

}  // namespace lyra::mir
