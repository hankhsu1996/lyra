#include "lyra/runtime/erased_value.hpp"

#include <type_traits>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

namespace {

template <typename T>
auto Read(void* handle) -> value::RuntimeValue {
  return value::RuntimeValue{*static_cast<const T*>(handle)};
}

}  // namespace

auto HandleOf(const value::RuntimeValue& value) -> const void* {
  return std::visit(
      [](const auto& held) -> const void* {
        using T = std::decay_t<decltype(held)>;
        if constexpr (std::is_same_v<T, value::Chandle>) {
          return held.Ptr();
        } else {
          return &held;
        }
      },
      value.value);
}

auto ValueOf(support::ValueDomain domain, void* handle) -> value::RuntimeValue {
  switch (domain) {
    case support::ValueDomain::kPacked:
      return Read<value::PackedArray>(handle);
    case support::ValueDomain::kString:
      return Read<value::String>(handle);
    case support::ValueDomain::kReal:
      return Read<value::Real>(handle);
    case support::ValueDomain::kShortReal:
      return Read<value::ShortReal>(handle);
    case support::ValueDomain::kChandle:
      return value::RuntimeValue{value::Chandle{handle}};
    case support::ValueDomain::kEmpty:
      return Read<value::Empty>(handle);
    case support::ValueDomain::kTuple:
      return Read<value::RuntimeTuple>(handle);
    case support::ValueDomain::kUnion:
      return Read<value::RuntimeUnion>(handle);
    case support::ValueDomain::kTaggedUnion:
      return Read<value::RuntimeTaggedUnion>(handle);
    case support::ValueDomain::kDynArray:
      return Read<value::RuntimeDynamicArray>(handle);
    case support::ValueDomain::kUnpackedArray:
      return Read<value::RuntimeUnpackedArray>(handle);
    case support::ValueDomain::kQueue:
      return Read<value::RuntimeQueue>(handle);
    case support::ValueDomain::kAssocArray:
      return Read<value::RuntimeAssociativeArray>(handle);
    case support::ValueDomain::kManagedRef:
      // A managed reference is not one of the alternatives an erased value
      // holds, so a container of them has no realization either and refuses
      // where it is built.
      throw InternalError(
          "erased value: a managed reference is not an erased value");
  }
  throw InternalError("erased value: unknown value domain");
}

}  // namespace lyra::runtime
