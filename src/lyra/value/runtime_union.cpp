#include "lyra/value/runtime_union.hpp"

#include <cstddef>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_union.hpp"

namespace lyra::value {

RuntimeUnion::RuntimeUnion(std::size_t index, AnyValue value)
    : BasicUnion(HeldMember(index, std::move(value))) {
}

RuntimeUnion::RuntimeUnion(HeldMember live) : BasicUnion(std::move(live)) {
}

auto RuntimeUnion::Component(std::size_t index) const -> const AnyValue& {
  if (index != Live().Index()) {
    throw SimulationError(
        "reading an unpacked-union member other than the one last written is "
        "undefined (LRM 7.3) and not yet supported on this backend; please "
        "open an issue asking for support");
  }
  return Live().Value();
}

void RuntimeUnion::SetComponent(std::size_t index, AnyValue value) {
  Live() = HeldMember(index, std::move(value));
}

}  // namespace lyra::value
