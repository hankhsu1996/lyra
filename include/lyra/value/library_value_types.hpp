#pragma once

#include <type_traits>

#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The type of each value the library itself defines, one per kind, which
// generated code names by symbol wherever it hands the library a value of one
// of these kinds together with its type. The first is a value held with the
// type it was written in, which is what an index of a wildcard-indexed array
// is (LRM 7.8.1).
extern "C" {
extern const ValueType lyra_rt_wildcard_index_value_type;
extern const ValueType lyra_rt_any_value_type;
extern const ValueType lyra_rt_string_value_type;
extern const ValueType lyra_rt_real_value_type;
extern const ValueType lyra_rt_shortreal_value_type;
extern const ValueType lyra_rt_chandle_value_type;
extern const ValueType lyra_rt_empty_value_type;
extern const ValueType lyra_rt_dynarray_value_type;
extern const ValueType lyra_rt_queue_value_type;
extern const ValueType lyra_rt_assocarray_value_type;
extern const ValueType lyra_rt_managedref_value_type;
}

// The library's type of a value of its own kind `T`.
template <typename T>
[[nodiscard]] auto LibraryTypeOf() -> const ValueType& {
  if constexpr (IntegralValue<T>) {
    return IntegralValueType::Of<T>().Type();
  } else if constexpr (std::is_same_v<T, AnyValue>) {
    return lyra_rt_wildcard_index_value_type;
  } else if constexpr (std::is_same_v<T, Any>) {
    return lyra_rt_any_value_type;
  } else if constexpr (std::is_same_v<T, String>) {
    return lyra_rt_string_value_type;
  } else if constexpr (std::is_same_v<T, Real>) {
    return lyra_rt_real_value_type;
  } else if constexpr (std::is_same_v<T, ShortReal>) {
    return lyra_rt_shortreal_value_type;
  } else if constexpr (std::is_same_v<T, Chandle>) {
    return lyra_rt_chandle_value_type;
  } else if constexpr (std::is_same_v<T, Empty>) {
    return lyra_rt_empty_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeDynamicArray>) {
    return lyra_rt_dynarray_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeQueue>) {
    return lyra_rt_queue_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeAssociativeArray>) {
    return lyra_rt_assocarray_value_type;
  } else {
    static_assert(std::is_same_v<T, ObjectRef>);
    return lyra_rt_managedref_value_type;
  }
}

}  // namespace lyra::value
