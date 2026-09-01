#include "lyra/lowering/hir_to_mir/expression/real_conversion.hpp"

#include <expected>
#include <utility>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerRealConversionCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& unit_lowerer = lowerer.Owner();
  auto& block = *frame.current_block;
  const std::vector<hir::ExprId> operands = RequiredOperands(c);
  auto operand_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(operands[0]), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id = block.exprs.Add(*std::move(operand_or));
  const mir::TypeId machine_int = unit_lowerer.Unit().builtins.machine_int64;

  // Reading a pattern back names the destination precision's own factory: the
  // integral operand yields the bits it spells, and the factory reinterprets
  // them rather than converting the number they would otherwise stand for.
  if (b.method == support::BuiltinFn::kFromBits) {
    const mir::ExprId bits = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kToInt64},
                    .arguments = {operand_id}},
            .type = machine_int});
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromBits,
                        .qualification =
                            mir::TypeQualifier{.type = result_type}},
                .arguments = {bits}},
        .type = result_type};
  }

  // The other direction answers in a machine integer -- the fraction dropped,
  // or the pattern itself -- which the destination's declared representation
  // then lands into, named as the type it is.
  const mir::ExprId read_out = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = b.method},
                  .arguments = {operand_id}},
          .type = machine_int});
  const mir::ExprId packed_type =
      mir::BuildPackedTypeRef(unit_lowerer.Unit(), block, result_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFromInt,
                      .qualification = mir::TypeQualifier{.type = result_type}},
              .arguments = {read_out, packed_type}},
      .type = result_type};
}

template auto LowerRealConversionCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::BuiltinMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerRealConversionCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::BuiltinMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
