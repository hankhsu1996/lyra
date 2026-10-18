#include "lyra/value/runtime_array_ordering.hpp"

#include <cstdint>

#include "lyra/base/function_ref.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/value/array_ordering.hpp"
#include "lyra/value/fixed_array_type.hpp"

namespace lyra::value {

void RuntimeArraySort(
    const FixedArrayType& type, void* array,
    const base::FunctionRef<support::OrderSignature>& before) {
  type.Permute(array, detail::SortedPositions(type.Count(), before));
}

void RuntimeArrayReverse(const FixedArrayType& type, void* array) {
  type.Permute(array, detail::ReversedPositions(type.Count()));
}

}  // namespace lyra::value
