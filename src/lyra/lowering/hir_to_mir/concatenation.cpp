#include "lyra/lowering/hir_to_mir/concatenation.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto JoinedBitsType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    std::span<const mir::ExprId> parts) -> mir::TypeId {
  std::uint64_t width = 0;
  mir::IntegralStateKind state_kind = mir::IntegralStateKind::kTwoState;
  for (const mir::ExprId part : parts) {
    const mir::IntegralType integral =
        unit.types.Get(block.exprs.Get(part).type).Integral();
    width += integral.bit_width;
    if (integral.state_kind == mir::IntegralStateKind::kFourState) {
      state_kind = mir::IntegralStateKind::kFourState;
    }
  }
  return mir::PackedVectorOf(unit.types, width, state_kind);
}

auto BuildBitsJoin(
    mir::CompilationUnit& unit, mir::Builder block, mir::LocalId joined,
    std::span<const mir::ExprId> parts) -> mir::ExprId {
  const mir::TypeId type = JoinedBitsType(unit, block.Body(), parts);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = joined,
          .init = mir::BuildIntegralLiteral(
              unit, block, type, mir::IntegralConstant{})});
  // Each part is written in the order the source lists it, below the one
  // before it, so the last takes the least significant bits.
  std::uint64_t above = unit.types.Get(type).Integral().bit_width;
  for (const mir::ExprId part : parts) {
    const mir::TypeId part_type = block.Get(part).type;
    above -= unit.types.Get(part_type).Integral().bit_width;
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(BuildStoreExpr(
                unit, block,
                AccessPath{
                    .owner = block.LocalPlace(joined, type),
                    .descent = {DescentStep{
                        .to =
                            StepToBits{
                                .start = BuildConstantPosition(
                                    unit, block,
                                    static_cast<std::int64_t>(above))},
                        .part_type = part_type}}},
                part))});
  }
  return block.ReadLocal(joined, type);
}

auto BuildBitsJoin(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const mir::ExprId> parts) -> mir::ExprId {
  mir::Builder block = frame.Builder();
  return BuildBitsJoin(
      unit, block,
      frame.bindings->DeclareAnonymous(
          JoinedBitsType(unit, block.Body(), parts)),
      parts);
}

auto BuildStringJoin(
    const mir::CompilationUnit& unit, mir::Builder block,
    std::span<const mir::ExprId> parts) -> mir::ExprId {
  if (parts.empty()) {
    throw InternalError("BuildStringJoin: a join has at least one part");
  }
  if (parts.size() == 1) {
    return parts.front();
  }
  const mir::TypeId string = unit.builtins.string;
  const mir::ExprId listed = block.Add(
      mir::Expr{
          .data =
              mir::CompositeExpr{
                  .parts =
                      std::vector<mir::ExprId>(parts.begin(), parts.end())},
          .type = mir::MachineArrayOf(unit.types, string, parts.size())});
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kConcatStrings, std::nullopt,
          {listed}, string));
}

}  // namespace lyra::lowering::hir_to_mir
