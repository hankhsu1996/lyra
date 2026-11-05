#pragma once

#include <variant>

#include "lyra/hir/structural_data_object.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

// What a net's data type gives its install (LRM 6.7.1): nothing for an integral
// type, whose positions the type itself counts, or a value of an unpacked
// aggregate type, whose shape the net takes. A type no net type resolves gives
// a value of it too, and makes the net one of a nettype with no resolution
// function (LRM 6.6.7), which is what an input port of such a type written
// with no port kind is (LRM 23.2.2.3).
struct IntegralNetData {};
struct AggregateNetData {
  mir::ExprId prototype;
};
struct UnresolvedNetData {
  mir::ExprId prototype;
};
using NetData =
    std::variant<IntegralNetData, AggregateNetData, UnresolvedNetData>;

// The install of the net `target` names, declared as `net`: what its data type
// gives it, and what its declared net type states (LRM 6.6, Table 6-1) -- which
// resolution the net performs, named by the entry that installs a net of its
// data type, and the contribution the net type makes to the net's own
// resolution, as the scalar the net shows where nothing drives it and the
// strength it holds that scalar at (LRM 6.6.5, 6.6.6, 6.7.1). A net no net type
// resolves states none of these: what it shows is its one driver's, or the
// value it was handed.
[[nodiscard]] auto BuildNetInstall(
    const mir::CompilationUnit& unit, mir::Builder block,
    const hir::StructuralNetDecl& net, mir::ExprId target, const NetData& data,
    mir::TypeId void_type) -> mir::Expr;

// A strength as the operand a net install or a driver attach carries: the level
// on the scale LRM Table 28-7 numbers, written as a machine integer.
[[nodiscard]] auto BuildStrengthOperand(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::StrengthLevel level) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
