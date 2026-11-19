#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/value/array_type_bodies.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of a queue (LRM 7.10) as code compiled before any design existed is
// handed it: the numbers and the table every type states, then the type of
// its elements and how many it may hold (LRM 7.10.5), and the type's own
// bodies finding where a write through a position lands and making a value a
// count long. The element type and the bound are facts of the type the program
// declares rather than of any value, so a queue built at the type reads them
// here, and a write through a holder reaching an element asks the body here. A
// value of it is the core its elements are kept in, whose fields say where they
// lie.
//
// Generated code states one of these for each queue type it hands such code a
// queue of, as constant data laid out as this record is.
class QueueType {
 public:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  // LRM 7.10.5: the largest index the queue holds; below zero, none.
  [[nodiscard]] auto Bound() const -> std::int64_t {
    return bound_;
  }

  // LRM 7.10.1: where a write through the position `position` points at, into
  // the queue lying at `queue`, lands, which may append an element, as the
  // queue type's own body finds it.
  [[nodiscard]] auto ElementPlace(void* queue, const void* position) const
      -> FoundElement {
    FoundElement found{};
    element_place_(type_, queue, position, &found);
    return found;
  }

  // How many elements the queue lying at `queue` holds, and where the one at
  // the index `index`, which names one it holds, lies; an index naming none is
  // a bug of whoever asked, which this reports.
  [[nodiscard]] static auto Count(const void* queue) -> std::size_t;
  [[nodiscard]] static auto GetUnchecked(const void* queue, std::size_t index)
      -> const void*;
  [[nodiscard]] static auto GetUnchecked(void* queue, std::size_t index)
      -> void*;

  // The array lying at `array` made `count` elements long, built in `out`,
  // holding no value, as the type's own body makes it.
  void Resized(const void* array, std::size_t count, void* out) const {
    const auto counted = static_cast<std::int64_t>(count);
    resized_(type_, array, &counted, out);
  }

  // Where the type states each of its own parts, past the numbers every type
  // states, which code stating one as constant data places them by.
  [[nodiscard]] static constexpr auto ElementStatedAt() -> std::size_t {
    return offsetof(QueueType, element_);
  }
  [[nodiscard]] static constexpr auto BoundStatedAt() -> StatedAt {
    return {.offset = offsetof(QueueType, bound_), .bytes = sizeof(bound_)};
  }
  [[nodiscard]] static constexpr auto ElementPlaceStatedAt() -> std::size_t {
    return offsetof(QueueType, element_place_);
  }
  [[nodiscard]] static constexpr auto ResizedStatedAt() -> std::size_t {
    return offsetof(QueueType, resized_);
  }

 private:
  ValueType type_;
  const value::ElementType* element_;
  std::int64_t bound_;
  ElementPlaceFunction element_place_;
  ResizedFunction resized_;
};

static_assert(std::is_standard_layout_v<QueueType>);

// Any queue's elements answered by position, for what walks a value down to
// its leaves, each answer read off the queue type it is handed and the core it
// lies in. The table of every queue type names this one.
extern "C" {
extern const PartsWitnessTable lyra_rt_queue_parts_witness_table;
}

}  // namespace lyra::value
