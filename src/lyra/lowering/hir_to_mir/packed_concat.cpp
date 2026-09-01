#include "lyra/lowering/hir_to_mir/packed_concat.hpp"

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildPackedConcat(
    mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> runs) -> mir::ExprId {
  if (runs.empty()) {
    throw InternalError("BuildPackedConcat: a join has at least one run");
  }
  const auto shape_of = [&](mir::ExprId run) -> const mir::PackedArrayType& {
    return unit.types.Get(block.exprs.Get(run).type).PackedShape();
  };
  // Bits join two at a time, because the entry that composes them takes two.
  // The order is left to right, which is the order the runs were written in:
  // joining is associative over both the bit plane and the state domain, so the
  // chain and the single N-run join it stands for hold the same value. Each
  // step is as wide as what it has joined so far, and carries an X or a Z as
  // soon as one of those runs can.
  std::uint64_t width = shape_of(runs.front()).BitWidth();
  mir::IntegralStateKind state_kind = shape_of(runs.front()).state_kind;
  mir::ExprId joined = runs.front();
  for (std::size_t i = 1; i < runs.size(); ++i) {
    const mir::PackedArrayType& run = shape_of(runs[i]);
    width += run.BitWidth();
    if (run.state_kind == mir::IntegralStateKind::kFourState) {
      state_kind = mir::IntegralStateKind::kFourState;
    }
    joined = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kConcat},
                    .arguments = {joined, runs[i]}},
            .type = mir::PackedVectorOf(unit.types, width, state_kind)});
  }
  return joined;
}

}  // namespace lyra::lowering::hir_to_mir
