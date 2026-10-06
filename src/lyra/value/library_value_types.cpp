#include "lyra/value/library_value_types.hpp"

#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// An index type's own order where it keys an associative array (LRM 7.8),
// which for a handle is no SV operator at all; otherwise the type's `<`.
template <LyraValue T>
auto ValueTypeOf<T>::OrderBefore(const void* lhs, const void* rhs) const
    -> bool {
  if constexpr (requires { typename AssocKeyTraits<T>::Less; }) {
    return typename AssocKeyTraits<T>::Less{}(Of(lhs), Of(rhs));
  } else if constexpr (requires { Of(lhs) < Of(rhs); }) {
    return static_cast<bool>(Of(lhs) < Of(rhs));
  } else {
    detail::LacksOperation();
  }
}

template class ValueTypeOf<PackedArray>;
template class ValueTypeOf<String>;
template class ValueTypeOf<Real>;
template class ValueTypeOf<ShortReal>;
template class ValueTypeOf<Chandle>;
template class ValueTypeOf<Empty>;
template class ValueTypeOf<RuntimeUnion>;
template class ValueTypeOf<RuntimeTaggedUnion>;
template class ValueTypeOf<RuntimeDynamicArray>;
template class ValueTypeOf<RuntimeUnpackedArray>;
template class ValueTypeOf<RuntimeQueue>;
template class ValueTypeOf<RuntimeAssociativeArray>;
template class ValueTypeOf<ObjectRef>;

}  // namespace lyra::value

extern "C" {
constinit const lyra::value::ValueTypeOf<lyra::value::PackedArray>
    lyra_rt_packed_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::String>
    lyra_rt_string_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::Real>
    lyra_rt_real_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::ShortReal>
    lyra_rt_shortreal_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::Chandle>
    lyra_rt_chandle_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::Empty>
    lyra_rt_empty_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeUnion>
    lyra_rt_union_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeTaggedUnion>
    lyra_rt_tagged_union_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeDynamicArray>
    lyra_rt_dynarray_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeUnpackedArray>
    lyra_rt_unpackedarray_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeQueue>
    lyra_rt_queue_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::RuntimeAssociativeArray>
    lyra_rt_assocarray_value_type;
constinit const lyra::value::ValueTypeOf<lyra::value::ObjectRef>
    lyra_rt_managedref_value_type;
}
