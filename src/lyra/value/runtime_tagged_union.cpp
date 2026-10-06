#include "lyra/value/runtime_tagged_union.hpp"

#include <cstddef>
#include <format>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_union.hpp"
#include "lyra/value/runtime_union.hpp"

namespace lyra::value {

RuntimeTaggedUnion::RuntimeTaggedUnion(std::size_t tag, AnyValue payload)
    : BasicUnion(HeldMember(tag, std::move(payload))) {
}

RuntimeTaggedUnion::RuntimeTaggedUnion(HeldMember live)
    : BasicUnion(std::move(live)) {
}

auto RuntimeTaggedUnion::Tag() const -> std::size_t {
  return Live().Index();
}

void RuntimeTaggedUnion::RequireTagged(
    std::size_t index, const char* access) const {
  if (index != Tag()) {
    throw SimulationError(
        std::format(
            "{} a tagged union member inconsistent with the current tag "
            "(LRM 11.9)",
            access));
  }
}

auto RuntimeTaggedUnion::Component(std::size_t index) const -> const AnyValue& {
  RequireTagged(index, "read of");
  return Live().Value();
}

void RuntimeTaggedUnion::SetComponent(std::size_t index, AnyValue value) {
  RequireTagged(index, "write to");
  Live() = HeldMember(index, std::move(value));
}

}  // namespace lyra::value
