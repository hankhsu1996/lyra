#pragma once

#include <cstdint>

namespace lyra::value {

// Compile-time description of a PackedArray's shape: width, signedness, and
// state domain. Used to declare the base type of any value that lives in a
// PackedArray (e.g., enum base types). PackedArray itself stores the same
// triple as runtime members; this struct is the constexpr-friendly form for
// generated code to declare its base type once.
struct PackedType {
  std::int32_t width;
  bool is_signed;
  bool is_four_state;
};

}  // namespace lyra::value
