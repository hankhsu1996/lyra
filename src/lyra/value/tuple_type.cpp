#include "lyra/value/tuple_type.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
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

void TupleType::BuildEachComponentDefault(void* laid_out) const {
  const std::span<const TupleComponent> stated = Components();
  std::size_t built = 0;
  const auto end_built = [&](const std::size_t* count) {
    for (std::size_t i = 0; i < *count; ++i) {
      stated[i].type->Destroy(ComponentAt(laid_out, i));
    }
  };
  std::unique_ptr<const std::size_t, decltype(end_built)> unwinding(
      &built, end_built);
  for (; built < stated.size(); ++built) {
    stated[built].type->BuildDefault(ComponentAt(laid_out, built));
  }
  unwinding.release();
}

void TupleType::ClearEachComponentAboveWidths(void* laid_out) const {
  const std::span<const TupleComponent> stated = Components();
  for (std::size_t i = 0; i < stated.size(); ++i) {
    stated[i].type->ClearAboveWidths(ComponentAt(laid_out, i));
  }
}

}  // namespace lyra::value
