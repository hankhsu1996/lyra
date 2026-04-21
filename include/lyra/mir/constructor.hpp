#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

// Body-owned construction entry routine.
//
// Exactly one Constructor per mir::ModuleBody. Reuses the generic MIR
// executable substrate (basic blocks, statements, operands, rvalues, temps)
// but is not a user-callable Function and not a simulation Process. It has
// no signature, no ABI contract, no parameters, and no decision-site
// tracking. The constructor body is lowered and verified; it is not yet
// consumed by LLVM or runtime in this cut.
struct Constructor {
  BasicBlockId entry;
  std::vector<BasicBlock> blocks;
  std::vector<TypeId> local_types;
  std::vector<TempMetadata> temp_metadata;
  common::OriginId origin = common::OriginId::Invalid();
  uint64_t materialize_count = 0;
};

}  // namespace lyra::mir
