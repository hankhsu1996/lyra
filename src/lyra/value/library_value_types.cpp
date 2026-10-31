#include "lyra/value/library_value_types.hpp"

#include <cstdint>
#include <memory>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
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

namespace {

// The language's operations on a whole container of a kind the library holds,
// each comparing or streaming an element by the element's operations, which
// the container type's operations name.
template <typename C>
struct ContainerOperations {
  static auto Of(const void* value) -> const C& {
    return *static_cast<const C*>(value);
  }

  static auto Equal(
      const ValueOperations& self, const void* lhs, const void* rhs)
      -> FourStateBit {
    return Of(lhs).Equal(Of(rhs), *self.element);
  }
  static auto CaseEqual(
      const ValueOperations& self, const void* lhs, const void* rhs) -> bool {
    return Of(lhs).CaseEqual(Of(rhs), *self.element).IsTruthy();
  }
  static auto HasUnknown(const ValueOperations& self, const void* value)
      -> bool {
    return Of(value).HasUnknown(*self.element);
  }
  static void BitstreamWidth(
      const ValueOperations& self, const void* value, void* out) {
    std::construct_at(
        static_cast<Int*>(out), Of(value).BitstreamWidth(*self.element));
  }
  static void CountBits(
      const ValueOperations& self, const void* value,
      const ConstIntegralView& control_bits, void* out) {
    std::construct_at(
        static_cast<Int*>(out),
        Of(value).CountBits(control_bits, *self.element));
  }
  static auto WriteToStream(
      const ValueOperations& self, const void* value,
      const IntegralView& stream, std::uint64_t filled) -> std::uint64_t {
    return Of(value).WriteToStream(stream, filled, *self.element);
  }
  // An associative array is no stream's destination (LRM 11.4.14.3), so a
  // program reading one out of a stream is one the front end refused.
  static auto ReadFromStream(
      const ValueOperations& self, const ConstIntegralView& stream,
      std::uint64_t taken, std::uint64_t until, void* out) -> std::uint64_t {
    if constexpr (requires(C& c) {
                    c.ReadFromStream(stream, taken, until, *self.element);
                  }) {
      return static_cast<C*>(out)->ReadFromStream(
          stream, taken, until, *self.element);
    } else {
      LacksOperation();
    }
  }
};

}  // namespace

extern "C" {
constinit const ValueType lyra_rt_wildcard_index_value_type =
    LibraryValueType<AnyValue>();
constinit const ValueType lyra_rt_any_value_type = LibraryValueType<Any>();
constinit const ValueType lyra_rt_string_value_type =
    LibraryValueType<String>();
constinit const ValueType lyra_rt_bitvec_value_type =
    LibraryValueType<BitVec>();
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

constinit const OperationFunctions lyra_rt_string_operation_functions =
    kOperationFunctionsFor<OperationsOf<String>>;
constinit const OperationFunctions lyra_rt_real_operation_functions =
    kOperationFunctionsFor<OperationsOf<Real>>;
constinit const OperationFunctions lyra_rt_shortreal_operation_functions =
    kOperationFunctionsFor<OperationsOf<ShortReal>>;
constinit const OperationFunctions lyra_rt_chandle_operation_functions =
    kOperationFunctionsFor<OperationsOf<Chandle>>;
constinit const OperationFunctions lyra_rt_managedref_operation_functions =
    kOperationFunctionsFor<OperationsOf<ObjectRef>>;
constinit const OperationFunctions lyra_rt_dynarray_operation_functions =
    kOperationFunctionsFor<ContainerOperations<RuntimeDynamicArray>>;
constinit const OperationFunctions lyra_rt_queue_operation_functions =
    kOperationFunctionsFor<ContainerOperations<RuntimeQueue>>;
constinit const OperationFunctions lyra_rt_assocarray_operation_functions =
    kOperationFunctionsFor<ContainerOperations<RuntimeAssociativeArray>>;

constinit const ValueOperations lyra_rt_string_value_operations{
    .functions = &lyra_rt_string_operation_functions,
    .type = &lyra_rt_string_value_type,
    .element = nullptr};
constinit const ValueOperations lyra_rt_real_value_operations{
    .functions = &lyra_rt_real_operation_functions,
    .type = &lyra_rt_real_value_type,
    .element = nullptr};
constinit const ValueOperations lyra_rt_shortreal_value_operations{
    .functions = &lyra_rt_shortreal_operation_functions,
    .type = &lyra_rt_shortreal_value_type,
    .element = nullptr};
constinit const ValueOperations lyra_rt_chandle_value_operations{
    .functions = &lyra_rt_chandle_operation_functions,
    .type = &lyra_rt_chandle_value_type,
    .element = nullptr};
constinit const ValueOperations lyra_rt_managedref_value_operations{
    .functions = &lyra_rt_managedref_operation_functions,
    .type = &lyra_rt_managedref_value_type,
    .element = nullptr};
}

}  // namespace lyra::value
