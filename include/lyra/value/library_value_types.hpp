#pragma once

#include <type_traits>

#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unmanaged_ref.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/value_witnesses_of.hpp"

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
extern const ValueType lyra_rt_bitvec_value_type;
extern const ValueType lyra_rt_real_value_type;
extern const ValueType lyra_rt_shortreal_value_type;
extern const ValueType lyra_rt_unmanagedref_value_type;
extern const ValueType lyra_rt_managedref_value_type;

// The functions of the language's operations on a value of each kind that can
// be compared or streamed, and for a kind no other type parameterizes, the
// whole of its operations. A container's operations name its element's, so
// generated code states them for each container type, naming these functions.
extern const OperationFunctions lyra_rt_string_operation_functions;
extern const OperationFunctions lyra_rt_real_operation_functions;
extern const OperationFunctions lyra_rt_shortreal_operation_functions;
extern const OperationFunctions lyra_rt_unmanagedref_operation_functions;
extern const OperationFunctions lyra_rt_managedref_operation_functions;
extern const ValueOperations lyra_rt_string_value_operations;
extern const ValueOperations lyra_rt_real_value_operations;
extern const ValueOperations lyra_rt_shortreal_value_operations;
extern const ValueOperations lyra_rt_unmanagedref_value_operations;
extern const ValueOperations lyra_rt_managedref_value_operations;
}

// The type of a value of `T` as the run names it: the library's own for a kind
// it holds, and for any other the one stated for `T`, so a type has one record
// wherever a value of it is handed over with its type.
template <LyraValue T>
[[nodiscard]] auto LibraryTypeOf() -> const ValueType& {
  if constexpr (IntegralValue<T>) {
    return IntegralValueType::Of<T>().Type();
  } else if constexpr (std::is_same_v<T, AnyValue>) {
    return lyra_rt_wildcard_index_value_type;
  } else if constexpr (std::is_same_v<T, Any>) {
    return lyra_rt_any_value_type;
  } else if constexpr (std::is_same_v<T, String>) {
    return lyra_rt_string_value_type;
  } else if constexpr (std::is_same_v<T, BitVec>) {
    return lyra_rt_bitvec_value_type;
  } else if constexpr (std::is_same_v<T, Real>) {
    return lyra_rt_real_value_type;
  } else if constexpr (std::is_same_v<T, ShortReal>) {
    return lyra_rt_shortreal_value_type;
  } else if constexpr (std::is_same_v<T, UnmanagedRef>) {
    return lyra_rt_unmanagedref_value_type;
  } else if constexpr (std::is_same_v<T, ObjectRef>) {
    return lyra_rt_managedref_value_type;
  } else {
    return kValueTypeOf<T>;
  }
}

// Where that type lies, which a call naming the type of a value hands on, as
// Rust's `type_id::<T>()` answers the type it is called at.
template <LyraValue T>
[[nodiscard]] auto TypeRecordOf() -> const ValueType* {
  return &LibraryTypeOf<T>();
}

}  // namespace lyra::value
