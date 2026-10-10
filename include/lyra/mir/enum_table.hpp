#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The members `enumeration` declares, as the table the questions LRM 6.19.5
// and 6.24.2 ask of a value are answered against. Naming the table is what puts
// it in the unit.
[[nodiscard]] auto BuildEnumTableRef(
    const CompilationUnit& unit, Block& block, TypeId enumeration) -> ExprId;

}  // namespace lyra::mir
