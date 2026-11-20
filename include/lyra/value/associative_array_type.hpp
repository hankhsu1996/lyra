#pragma once

#include <cstddef>
#include <type_traits>

#include "lyra/value/array_type_bodies.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of an associative array (LRM 7.8) as code compiled before any
// design existed is handed it: the numbers and the table every type states,
// then the types of its elements and of its index, and the type's own body
// finding where a write through an index lands, which may make the entry the
// index names (LRM 7.8.7). A write through a holder reaching an element asks
// the body here, and what loads a memory file into one builds its indices at
// the index type. A value of it is the core its entries are kept in.
//
// Generated code states one of these for each associative array type it hands
// such code a value of, as constant data laid out as this record is.
class AssociativeArrayType {
 public:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }
  [[nodiscard]] auto IndexType() const -> const ValueType& {
    return *index_;
  }

  // LRM 7.8.6, 7.8.7: where a write through the index `index` points at,
  // into the array lying at `array`, lands, making the entry where it names
  // none, and none where the index holds x or z, as the array type's own body
  // finds it.
  [[nodiscard]] auto ElementPlace(void* array, const void* index) const
      -> FoundElement {
    FoundElement found{};
    element_place_(type_, array, index, &found);
    return found;
  }

  // Where the type states each of its own parts, past the numbers every type
  // states, which code stating one as constant data places them by.
  [[nodiscard]] static constexpr auto ElementStatedAt() -> std::size_t {
    return offsetof(AssociativeArrayType, element_);
  }
  [[nodiscard]] static constexpr auto IndexStatedAt() -> std::size_t {
    return offsetof(AssociativeArrayType, index_);
  }
  [[nodiscard]] static constexpr auto ElementPlaceStatedAt() -> std::size_t {
    return offsetof(AssociativeArrayType, element_place_);
  }

 private:
  ValueType type_;
  const value::ElementType* element_;
  const ValueType* index_;
  ElementPlaceFunction element_place_;
};

static_assert(std::is_standard_layout_v<AssociativeArrayType>);

}  // namespace lyra::value
