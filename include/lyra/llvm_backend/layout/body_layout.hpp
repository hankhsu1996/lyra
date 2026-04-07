#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/llvm_backend/layout/storage_types.hpp"

namespace lyra::lowering::mir_to_llvm {

// Per-body byte layout in the BodyByteOffset domain.
// All offsets are body-relative (from the start of the instance's
// inline storage region). No design-global addressing.
struct BodyLayout {
  // Per-slot inline byte offset, indexed by body-local slot ordinal.
  std::vector<BodyByteOffset> inline_offsets;
  // Per-slot appendix byte offset (for kOwnedContainer backing data).
  // nullopt for slots that are not owned containers.
  std::vector<std::optional<BodyByteOffset>> appendix_offsets;
  uint64_t inline_region_size = 0;
  uint64_t appendix_region_size = 0;
};

}  // namespace lyra::lowering::mir_to_llvm
