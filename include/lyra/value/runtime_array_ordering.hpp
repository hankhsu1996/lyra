#pragma once

#include <cstdint>

#include "lyra/base/function_ref.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/value/array_ordering.hpp"
#include "lyra/value/fixed_array_type.hpp"

// LRM 7.12.2 ordering over the containers the library holds. A sort is handed
// the order its elements go in -- told two positions, whether the element at
// the first comes before the one at the second -- which the program answers,
// so the library moves elements it never reads.
namespace lyra::value {

template <typename Container>
void RuntimeArraySort(
    Container& receiver,
    const base::FunctionRef<support::OrderSignature>& before) {
  receiver.Permute(detail::SortedPositions(receiver.Count(), before));
}
template <typename Container>
void RuntimeArrayReverse(Container& receiver) {
  receiver.Permute(detail::ReversedPositions(receiver.Count()));
}

// The same two over a fixed-size unpacked array of `type` where it lies at
// `array`, which is its elements and no object a method could be asked of.
void RuntimeArraySort(
    const FixedArrayType& type, void* array,
    const base::FunctionRef<support::OrderSignature>& before);
void RuntimeArrayReverse(const FixedArrayType& type, void* array);

}  // namespace lyra::value
