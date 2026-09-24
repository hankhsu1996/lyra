#include "lyra/runtime/erased_value.hpp"

#include <memory>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

namespace {

template <typename T>
auto Take(void* storage) -> value::RuntimeValue {
  auto* built = static_cast<T*>(storage);
  value::RuntimeValue taken{std::move(*built)};
  std::destroy_at(built);
  return taken;
}

}  // namespace

auto HandleOf(const value::RuntimeValue& value) -> const void* {
  return std::visit(
      [](const auto& held) -> const void* { return &held; }, value.value);
}

auto TakeValue(support::ValueDomain domain, void* storage)
    -> value::RuntimeValue {
  switch (domain) {
    case support::ValueDomain::kPacked:
      return Take<value::PackedArray>(storage);
    case support::ValueDomain::kString:
      return Take<value::String>(storage);
    case support::ValueDomain::kReal:
      return Take<value::Real>(storage);
    case support::ValueDomain::kShortReal:
      return Take<value::ShortReal>(storage);
    case support::ValueDomain::kChandle:
      return Take<value::Chandle>(storage);
    case support::ValueDomain::kEmpty:
      return Take<value::Empty>(storage);
    case support::ValueDomain::kTuple:
      return Take<value::RuntimeTuple>(storage);
    case support::ValueDomain::kUnion:
      return Take<value::RuntimeUnion>(storage);
    case support::ValueDomain::kTaggedUnion:
      return Take<value::RuntimeTaggedUnion>(storage);
    case support::ValueDomain::kDynArray:
      return Take<value::RuntimeDynamicArray>(storage);
    case support::ValueDomain::kUnpackedArray:
      return Take<value::RuntimeUnpackedArray>(storage);
    case support::ValueDomain::kQueue:
      return Take<value::RuntimeQueue>(storage);
    case support::ValueDomain::kAssocArray:
      return Take<value::RuntimeAssociativeArray>(storage);
    case support::ValueDomain::kManagedRef:
      return Take<value::ManagedRef>(storage);
  }
  throw InternalError("erased value: unknown value domain");
}

}  // namespace lyra::runtime
