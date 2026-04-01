#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/integral_constant.hpp"

namespace lyra::lowering::mir_to_llvm {

struct SlotStorageSpec;

// Lower an IntegralConstant into canonical storage bytes for a packed
// storage spec. Appends exactly spec.TotalByteSize() bytes to out.
//
// For 2-state packed: writes value bits into lane bytes.
// For 4-state packed: writes value bits to known lane, unknown bits
// to unknown lane at canonical offsets.
//
// Invariant: spec must be PackedStorageSpec. Other spec kinds are not
// supported for parameter initialization (params are always packed
// scalars in current Lyra).
void LowerIntegralConstantToCanonicalBytes(
    const IntegralConstant& value, const SlotStorageSpec& spec,
    std::vector<uint8_t>& out);

}  // namespace lyra::lowering::mir_to_llvm
