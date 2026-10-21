#include "lyra/value/library_value_types.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/value_witnesses_of.hpp"

namespace lyra::value {

void LacksOperation() {
  throw InternalError(
      "value type: an operation is asked of a type the language does not "
      "define it for -- please report this as a bug");
}

void StreamNotYetSupported() {
  throw SimulationError(
      "reading this value as a stream of bits, or building one from it, is "
      "not yet supported with '--backend llvm'");
}

extern "C" {
constinit const ValueType lyra_rt_wildcard_index_value_type =
    LibraryValueType<AnyValue>();
constinit const ValueType lyra_rt_any_value_type = LibraryValueType<Any>();
constinit const ValueType lyra_rt_string_value_type =
    LibraryValueType<String>();
constinit const ValueType lyra_rt_real_value_type = LibraryValueType<Real>();
constinit const ValueType lyra_rt_shortreal_value_type =
    LibraryValueType<ShortReal>();
constinit const ValueType lyra_rt_chandle_value_type =
    LibraryValueType<Chandle>();
constinit const ValueType lyra_rt_empty_value_type = LibraryValueType<Empty>();
constinit const ValueType lyra_rt_dynarray_value_type =
    LibraryValueType<RuntimeDynamicArray>();
constinit const ValueType lyra_rt_queue_value_type =
    LibraryValueType<RuntimeQueue>();
constinit const ValueType lyra_rt_assocarray_value_type =
    LibraryValueType<RuntimeAssociativeArray>();
constinit const ValueType lyra_rt_managedref_value_type =
    LibraryValueType<ObjectRef>();
}

}  // namespace lyra::value
