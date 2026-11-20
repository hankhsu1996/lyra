#include "lyra/value/dynamic_array_type.hpp"

#include <cstddef>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/container_core.hpp"

namespace lyra::value {

namespace {

auto CoreOf(const void* array) -> const DynamicArrayCore& {
  return *static_cast<const DynamicArrayCore*>(array);
}

}  // namespace

auto DynamicArrayType::Count(const void* array) -> std::size_t {
  return CoreOf(array).count;
}

auto DynamicArrayType::GetUnchecked(const void* array, std::size_t index)
    -> const void* {
  const DynamicArrayCore& core = CoreOf(array);
  if (index >= core.count) {
    throw InternalError(
        "the element of a dynamic array is asked at an index past its last "
        "-- please report this as a bug");
  }
  return DynamicArrayPlace(core, core.data, index);
}

auto DynamicArrayType::GetUnchecked(void* array, std::size_t index) -> void* {
  const DynamicArrayCore& core = CoreOf(array);
  if (index >= core.count) {
    throw InternalError(
        "the element of a dynamic array is asked at an index past its last "
        "-- please report this as a bug");
  }
  return DynamicArrayPlace(core, core.data, index);
}

}  // namespace lyra::value
