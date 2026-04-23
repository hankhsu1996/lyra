#pragma once

#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

struct UnitLoweringState {
  std::vector<mir::TypeId> type_map;
  std::vector<mir::MemberId> var_map;
};

struct ProcessLoweringState {
  std::vector<mir::ExprId> expr_map;
  std::vector<mir::StmtId> stmt_map;
};

}  // namespace lyra::lowering::hir_to_mir
