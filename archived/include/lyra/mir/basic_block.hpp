#pragma once

#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

// Block parameter: defines an SSA temp at block entry.
// Values are passed from predecessors via terminator edge args.
// This models PHI-like semantics for SSA construction at CFG joins.
struct BlockParam {
  int temp_id;  // SSA temp id defined at block entry
  TypeId type;  // Type of the incoming value
};

struct BasicBlock {
  // Block parameters: SSA temps defined at block entry.
  // Values come from predecessor edge args (Jump::args,
  // Branch::then_args/else_args). Empty for blocks without incoming SSA values.
  std::vector<BlockParam> params;

  // Ordered, side-effecting statements (no control flow)
  std::vector<Statement> statements;

  // Exactly one terminator, always last
  Terminator terminator;
};

}  // namespace lyra::mir
