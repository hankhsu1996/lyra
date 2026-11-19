#pragma once

#include <cstdint>
#include <utility>

namespace lyra::value {

// Which of the records the library reads a type through this one opens, where
// it opens one: an integral type's (LRM 6.11), whose values a facility
// compiled for every type reads the bits of, a fixed-size unpacked array
// type's (LRM 7.4.2), and a dynamic array type's and a queue type's (LRM 7.5,
// 7.10), whose element a write through a holder reaches. Every other type is
// its table and its numbers alone.
enum class ValueTypeKind : std::uint32_t {
  kIntegral,
  kFixedArray,
  kDynamicArray,
  kQueue,
  kOther,
};

// Whether a value of `kind` is an array whose size changes at run time (LRM
// 7.5, 7.10), and so moves and ends its elements while it lies where it is
// itself: a reference to an element goes on naming it while the array grows,
// shrinks or is replaced around it, so the position it was taken at says
// nothing of where the element is later (LRM 13.5.2, 7.10.3).
[[nodiscard]] constexpr auto ChangesSizeAtRunTime(ValueTypeKind kind) -> bool {
  switch (kind) {
    case ValueTypeKind::kDynamicArray:
    case ValueTypeKind::kQueue:
      return true;
    case ValueTypeKind::kIntegral:
    case ValueTypeKind::kFixedArray:
    case ValueTypeKind::kOther:
      return false;
  }
  std::unreachable();
}

// Whether a write to the element one past the last of a value of `kind`
// appends one (LRM 7.10.1), which a queue's does and a dynamic array's, which
// is discarded, does not (LRM 7.5).
[[nodiscard]] constexpr auto AppendsPastItsLast(ValueTypeKind kind) -> bool {
  switch (kind) {
    case ValueTypeKind::kQueue:
      return true;
    case ValueTypeKind::kIntegral:
    case ValueTypeKind::kFixedArray:
    case ValueTypeKind::kDynamicArray:
    case ValueTypeKind::kOther:
      return false;
  }
  std::unreachable();
}

}  // namespace lyra::value
