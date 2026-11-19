#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/value/value_type.hpp"

namespace lyra::value {

// What a write through a position into a run-time-sized array finds (LRM
// 7.10.1): where the element lies, none where the position names none, and
// whether the write made it there. It is laid out as the pair the array type's
// own body answers, an address followed by a truth.
struct FoundElement {
  void* element;
  bool made;
};

static_assert(std::is_standard_layout_v<FoundElement>);
static_assert(offsetof(FoundElement, made) == sizeof(void*));

// The bodies of its own a run-time-sized array's type states for code
// compiled before any design existed, each handed the type first as every
// function a type states is: where a write through the position `position`
// points at lands in the array lying at `array`; and the array lying at
// `array` made the count `count` points at long, built in `out`, holding no
// value, kept as far as it reaches and grown by element defaults (LRM 7.5.1),
// a queue then held to its bound (LRM 7.10.5).
using ElementPlaceFunction = void (*)(
    const ValueType& type, void* array, const void* position,
    FoundElement* found);
using ResizedFunction = void (*)(
    const ValueType& type, const void* array, const std::int64_t* count,
    void* out);

}  // namespace lyra::value
