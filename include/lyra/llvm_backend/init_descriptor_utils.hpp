#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/integral_constant.hpp"

namespace lyra::lowering::mir_to_llvm {

struct SlotStorageSpec;
class StorageSpecArena;

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

// Lower a general Constant to canonical storage bytes for a target
// slot spec. Dispatches on ConstantValue variant and target storage
// spec. This is the serialization boundary where constant/storage
// specialization belongs.
//
// Supported combinations:
//   IntegralConstant + PackedStorageSpec
//   RealConstant + FloatStorageSpec
//   StructConstant + StructStorageSpec (recursive field lowering)
//   ArrayConstant + ArrayStorageSpec (recursive element lowering)
//
// Unsupported combinations (explicit InternalError):
//   StringConstant + HandleStorageSpec (requires runtime string pool)
//   NullConstant (no storage representation)
//   Any other mismatch
void LowerConstantToCanonicalBytes(
    const Constant& constant, const SlotStorageSpec& spec,
    const ConstantArena& constant_arena, const StorageSpecArena& storage_arena,
    std::vector<uint8_t>& out);

}  // namespace lyra::lowering::mir_to_llvm
