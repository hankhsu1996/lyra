#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <type_traits>

#include "lyra/value/value_type.hpp"

namespace lyra::value {

// Where one component of a tuple sits in the tuple's storage, and the type it
// is a value of.
struct TupleComponent {
  std::uint32_t offset;
  const ValueType* type;
};

// The type of one tuple: an unpacked structure (LRM 7.2), or a call's answer,
// its result together with its `output` and `inout` arguments (LRM 13.5) --
// the numbers and the table every type states, then where each component
// lies. Only the code that laid a tuple type out can state its operations, so
// every one is generated there, one per type, as constant data laid out as
// this record is.
class TupleType {
 public:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
  [[nodiscard]] auto Components() const -> std::span<const TupleComponent> {
    return {components_, count_};
  }

  // Where component `index` of a tuple of this type laid out at `laid_out`
  // lies, which is that component's handle: a component laid out by its type
  // -- a structure, a union, a fixed-size array -- has its bytes there, and
  // every other component its object.
  [[nodiscard]] auto ComponentAt(void* laid_out, std::size_t index) const
      -> void*;
  [[nodiscard]] auto ComponentAt(const void* laid_out, std::size_t index) const
      -> const void*;

  // Where the type states how many components it lists and where the list
  // is, past the numbers every type states.
  [[nodiscard]] static constexpr auto CountStatedAt() -> StatedAt {
    return {.offset = offsetof(TupleType, count_), .bytes = sizeof(count_)};
  }
  [[nodiscard]] static constexpr auto ComponentsStatedAt() -> std::size_t {
    return offsetof(TupleType, components_);
  }

 private:
  ValueType type_;
  std::uint32_t count_;
  const TupleComponent* components_;
};

static_assert(std::is_standard_layout_v<TupleType>);

}  // namespace lyra::value
