#include "lyra/value/tuple_type.hpp"

#include <cstddef>
#include <span>

#include "lyra/base/internal_error.hpp"

namespace lyra::value {

namespace {

// The component `index` a tuple of `type` lists.
auto Stated(const TupleType& type, std::size_t index) -> const TupleComponent& {
  const std::span<const TupleComponent> stated = type.Components();
  if (index >= stated.size()) {
    throw InternalError("TupleType: a component index is out of range");
  }
  return stated[index];
}

}  // namespace

auto TupleType::ComponentAt(void* laid_out, std::size_t index) const -> void* {
  return Offset(laid_out, Stated(*this, index).offset);
}

auto TupleType::ComponentAt(const void* laid_out, std::size_t index) const
    -> const void* {
  return Offset(laid_out, Stated(*this, index).offset);
}

}  // namespace lyra::value
