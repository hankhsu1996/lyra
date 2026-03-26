#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/runtime/body_realization_desc.hpp"

namespace lyra::lowering::mir_to_llvm {

class StorageSpecArena;
struct SlotStorageSpec;

// Flatten all 4-state X-encoding within a storage spec into leaf-level
// byte-write patch entries. Walks the spec tree recursively:
// - PackedStorageSpec (4-state): one patch at unknown-lane offset
// - StructStorageSpec: recurse per field
// - ArrayStorageSpec: recurse per element
// - Non-4-state specs: emit nothing
//
// base_rel_offset is added to all emitted offsets. For body descriptors
// this is the slot's body-relative byte offset. For package descriptors
// it is the slot's arena-relative byte offset.
//
// No dependency on TypeArena. Driven entirely by StorageSpecArena.
void FlattenFourStatePatches(
    const SlotStorageSpec& spec, const StorageSpecArena& arena,
    uint64_t base_rel_offset, std::vector<runtime::InitPatchEntry>& out);

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
