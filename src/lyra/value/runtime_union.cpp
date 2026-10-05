#include "lyra/value/runtime_union.hpp"

#include <cstddef>
#include <utility>

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
    RefuseReadOfAnotherMember();
  }
  return Live().Value();
}

void RuntimeUnion::SetComponent(std::size_t index, AnyValue value) {
  Live() = HeldMember(index, std::move(value));
}

}  // namespace lyra::value
