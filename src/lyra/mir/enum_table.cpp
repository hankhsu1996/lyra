#include "lyra/mir/enum_table.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

auto BuildEnumTableRef(
    const CompilationUnit& unit, Block& block, TypeId enumeration) -> ExprId {
  const auto* declared = unit.types.Get(enumeration).As<EnumType>();
  if (declared == nullptr) {
    throw InternalError("mir: only an enumeration declares members");
  }
  const EnumTableId table = unit.enum_tables.Intern(*declared);
  return block.exprs.Add(
      Expr{
          .data = ReferenceExpr{.target = EnumTableRef{.table = table}},
          .type = unit.builtins.enumeration});
}

}  // namespace lyra::mir
