#include "lyra/value/runtime_array_manipulation.hpp"

#include "lyra/value/runtime_value.hpp"

namespace lyra::value {

auto LocatorKeyLess(const RuntimeValue& a, const RuntimeValue& b) -> bool {
  return RuntimeValueOrderBefore(a, b);
}

auto LocatorKeySame(const RuntimeValue& a, const RuntimeValue& b) -> bool {
  return RuntimeValueBitIdentical(a, b);
}

}  // namespace lyra::value
