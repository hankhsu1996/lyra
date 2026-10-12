#pragma once

#include <cstdint>

#include "lyra/value/element_type.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of a queue (LRM 7.10) as code compiled before any design existed is
// handed it: the type of its elements, and how many it may hold (LRM 7.10.5).
// Both are facts of the type the program declares rather than of any value,
// so a queue built at the type reads them here and is handed neither.
//
// Generated code states one of these for each queue type it hands the library
// a queue of, as constant data of this class, so its layout is part of what
// the two sides agree on.
class QueueType {
 public:
  constexpr QueueType(const value::ElementType& element, std::int64_t bound)
      : element_(&element), bound_(bound) {
  }

  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  // LRM 7.10.5: the largest index the queue holds; below zero, none.
  [[nodiscard]] auto Bound() const -> std::int64_t {
    return bound_;
  }

  // Where the type states each of its own parts, which code stating a type as
  // constant data places them by.
  [[nodiscard]] auto ElementStatedAt() const -> StatedAt;
  [[nodiscard]] auto BoundStatedAt() const -> StatedAt;

 private:
  const value::ElementType* element_;
  std::int64_t bound_;
};

}  // namespace lyra::value
