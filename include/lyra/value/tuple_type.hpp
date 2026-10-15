#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/value_type.hpp"

namespace lyra::value {

// Where one component of a tuple sits in the tuple's storage, and the type it
// is a value of.
struct TupleComponent {
  std::uint32_t offset;
  const ValueType* type;
};

// The type of one tuple: an unpacked structure (LRM 7.2), or a call's answer,
// its result together with its `output` and `inout` arguments (LRM 13.5). Only
// the code that laid a tuple type out can state its operations, so every one
// is generated there, one per type, and nothing here builds one.
class TupleType : public ValueType {
 public:
  TupleType() = delete;

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

  // Builds at `laid_out` a tuple of this type with each component at what its
  // own type builds before anything writes it. A component that fails to
  // build ends the ones built before it.
  void BuildEachComponentDefault(void* laid_out) const;

  // Each component of a tuple of this type laid out at `laid_out` cleared
  // above its widths, which is all a tuple keeps clear.
  void ClearEachComponentAboveWidths(void* laid_out) const;

 private:
  std::uint32_t count_;
  const TupleComponent* components_;
};

}  // namespace lyra::value
