#include "lyra/value/tuple_type.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
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

extern "C" {
constinit const StepWitnessTable lyra_rt_tuple_step_witness_table{
    .part = [](const ValueType& type, const void* value,
               std::uint32_t step) -> ValuePart {
      // A tuple type opens with the numbers every type states, so the type
      // handed is the tuple type's own address.
      const auto& tuple = *std::bit_cast<const TupleType*>(&type);
      if (step >= tuple.Components().size()) {
        return {.at = nullptr, .type = nullptr};
      }
      return {
          .at = tuple.ComponentAt(value, step),
          .type = tuple.Components()[step].type};
    }};
}

}  // namespace lyra::value
