#pragma once

#include "lyra/value/chandle.hpp"
#include "lyra/value/empty.hpp"
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

extern template class ValueTypeOf<PackedArray>;
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
// of these kinds together with its type.
extern "C" {
extern const lyra::value::ValueTypeOf<lyra::value::PackedArray>
    lyra_rt_packed_value_type;
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
