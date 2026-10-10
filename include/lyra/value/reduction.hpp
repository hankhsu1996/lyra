#pragma once

#include <concepts>
#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::value {

// The five ways LRM 7.12.3 folds an array's values into one.
enum class Reduction : std::uint8_t { kSum, kProduct, kAnd, kOr, kXor };

// Two values of `T` folded by `reduction`. A reduction the type does not define
// -- a bitwise one over a real -- is one the front end does not admit.
template <typename T>
[[nodiscard]] auto Reduced(Reduction reduction, const T& a, const T& b) -> T {
  switch (reduction) {
    case Reduction::kSum:
      if constexpr (requires {
                      { a + b } -> std::same_as<T>;
                    }) {
        return a + b;
      }
      break;
    case Reduction::kProduct:
      if constexpr (requires {
                      { a * b } -> std::same_as<T>;
                    }) {
        return a * b;
      }
      break;
    case Reduction::kAnd:
      if constexpr (requires {
                      { a & b } -> std::same_as<T>;
                    }) {
        return a & b;
      }
      break;
    case Reduction::kOr:
      if constexpr (requires {
                      { a | b } -> std::same_as<T>;
                    }) {
        return a | b;
      }
      break;
    case Reduction::kXor:
      if constexpr (requires {
                      { a ^ b } -> std::same_as<T>;
                    }) {
        return a ^ b;
      }
      break;
  }
  throw InternalError(
      "value type: an LRM 7.12.3 reduction is asked of a type that does not "
      "define it -- please report this as a bug");
}

}  // namespace lyra::value
