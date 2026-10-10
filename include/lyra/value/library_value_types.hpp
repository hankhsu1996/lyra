#pragma once

#include <type_traits>

#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type_of.hpp"

namespace lyra::value {

extern template class ValueTypeOf<AnyValue>;
extern template class ValueTypeOf<String>;
extern template class ValueTypeOf<Real>;
extern template class ValueTypeOf<ShortReal>;
extern template class ValueTypeOf<Chandle>;
extern template class ValueTypeOf<Empty>;
extern template class ValueTypeOf<RuntimeUnion>;
extern template class ValueTypeOf<RuntimeTaggedUnion>;
extern template class ValueTypeOf<RuntimeDynamicArray>;
extern template class ValueTypeOf<RuntimeUnpackedArray>;
extern template class ValueTypeOf<RuntimeQueue>;
extern template class ValueTypeOf<RuntimeAssociativeArray>;
extern template class ValueTypeOf<ObjectRef>;

}  // namespace lyra::value

// The type of each value the library itself defines, one per kind, which
// generated code names by symbol wherever it hands the library a value of one
// of these kinds together with its type. The first is a value held with the
// type it was written in, which is what an index of a wildcard-indexed array
// is (LRM 7.8.1).
extern "C" {
extern const lyra::value::ValueTypeOf<lyra::value::AnyValue>
    lyra_rt_wildcard_index_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::String>
    lyra_rt_string_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::Real>
    lyra_rt_real_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::ShortReal>
    lyra_rt_shortreal_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::Chandle>
    lyra_rt_chandle_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::Empty>
    lyra_rt_empty_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeUnion>
    lyra_rt_union_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeTaggedUnion>
    lyra_rt_tagged_union_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeDynamicArray>
    lyra_rt_dynarray_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeUnpackedArray>
    lyra_rt_unpackedarray_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeQueue>
    lyra_rt_queue_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::RuntimeAssociativeArray>
    lyra_rt_assocarray_value_type;
extern const lyra::value::ValueTypeOf<lyra::value::ObjectRef>
    lyra_rt_managedref_value_type;
}

namespace lyra::value {

// The library's type of a value of its own kind `T`.
template <typename T>
[[nodiscard]] auto LibraryTypeOf() -> const ValueTypeOf<T>& {
  if constexpr (std::is_same_v<T, String>) {
    return lyra_rt_string_value_type;
  } else if constexpr (std::is_same_v<T, Real>) {
    return lyra_rt_real_value_type;
  } else if constexpr (std::is_same_v<T, ShortReal>) {
    return lyra_rt_shortreal_value_type;
  } else if constexpr (std::is_same_v<T, Chandle>) {
    return lyra_rt_chandle_value_type;
  } else if constexpr (std::is_same_v<T, Empty>) {
    return lyra_rt_empty_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeUnion>) {
    return lyra_rt_union_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeTaggedUnion>) {
    return lyra_rt_tagged_union_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeDynamicArray>) {
    return lyra_rt_dynarray_value_type;
  } else if constexpr (std::is_same_v<T, RuntimeUnpackedArray>) {
    return lyra_rt_unpackedarray_value_type;
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
