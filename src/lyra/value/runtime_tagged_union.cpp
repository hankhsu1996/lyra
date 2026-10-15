#include "lyra/value/runtime_tagged_union.hpp"

#include <cstddef>
#include <utility>

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

auto RuntimeTaggedUnion::Component(std::size_t index) const -> const AnyValue& {
  if (index != Tag()) {
    RefuseMemberTheTagDoesNotName();
  }
  return Live().Value();
}

void RuntimeTaggedUnion::SetComponent(std::size_t index, AnyValue value) {
  if (index != Tag()) {
    RefuseMemberTheTagDoesNotName();
  }
  Live() = HeldMember(index, std::move(value));
}

}  // namespace lyra::value
