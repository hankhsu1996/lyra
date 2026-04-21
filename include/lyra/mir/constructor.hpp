#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

// Body-owned construction entry routine.
//
// Exactly one Constructor per mir::ModuleBody. Reuses the generic MIR
// executable substrate (basic blocks, statements, operands, rvalues, temps)
// and carries real typed formals for this body's transmitted parameters.
// Each formal is a scope-local kLocal (mirror of mir::Function). Any
// binding into body-owned storage is an ordinary assignment statement at
// the top of the constructor body, not a side table.
struct Constructor {
  BasicBlockId entry;
  std::vector<BasicBlock> blocks;
  std::vector<TypeId> local_types;
  std::vector<TempMetadata> temp_metadata;
  FunctionSignature signature;
  // Formal index -> local slot index (same role as
  // Function::param_local_slots).
  std::vector<uint32_t> param_local_slots;
  common::OriginId origin = common::OriginId::Invalid();
  uint64_t materialize_count = 0;
};

}  // namespace lyra::mir
