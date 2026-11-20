#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/value/array_type_bodies.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of a dynamic array (LRM 7.5) as code compiled before any design
// existed is handed it: the numbers and the table every type states, then its
// element's type, and the type's own bodies finding where a write through a
// position lands and making a value a count long, which is what reaching an
// element through a holder and resizing one need --
// where a position names none, the write lands where no read reaches. A value
// of it is the core its elements are kept in, whose fields say where they lie.
//
// Generated code states one of these for each dynamic array type it hands a
// value of to such code, as constant data laid out as this record is.
class DynamicArrayType {
 public:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  // LRM 7.4.5: where a write through the position `position` points at, into
  // the array lying at `array`, lands, none where it names no element, as the
  // array type's own body finds it. An array never grows by being written.
  [[nodiscard]] auto ElementPlace(void* array, const void* position) const
      -> FoundElement {
    FoundElement found{};
    element_place_(type_, array, position, &found);
    return found;
  }

  // How many elements the array lying at `array` holds, and where the one at
  // the index `index`, which names one it holds, lies; an index naming none is
  // a bug of whoever asked, which this reports.
  [[nodiscard]] static auto Count(const void* array) -> std::size_t;
  [[nodiscard]] static auto GetUnchecked(const void* array, std::size_t index)
      -> const void*;
  [[nodiscard]] static auto GetUnchecked(void* array, std::size_t index)
      -> void*;

  // The array lying at `array` made `count` elements long, built in `out`,
  // holding no value, as the type's own body makes it.
  void Resized(const void* array, std::size_t count, void* out) const {
    const auto counted = static_cast<std::int64_t>(count);
    resized_(type_, array, &counted, out);
  }

  // Where the type states the address of its element's type, past the
  // numbers every type states.
  [[nodiscard]] static constexpr auto ElementStatedAt() -> std::size_t {
    return offsetof(DynamicArrayType, element_);
  }
  [[nodiscard]] static constexpr auto ElementPlaceStatedAt() -> std::size_t {
    return offsetof(DynamicArrayType, element_place_);
  }
  [[nodiscard]] static constexpr auto ResizedStatedAt() -> std::size_t {
    return offsetof(DynamicArrayType, resized_);
  }

 private:
  ValueType type_;
  const value::ElementType* element_;
  ElementPlaceFunction element_place_;
  ResizedFunction resized_;
};

static_assert(std::is_standard_layout_v<DynamicArrayType>);

}  // namespace lyra::value
