#include "lyra/lowering/hir_to_mir/expression/real_conversion.hpp"

#include <expected>
#include <utility>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
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
  const auto lower = [&](hir::ExprId source) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(source), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };

  // Reading a pattern back names the destination precision's own factory: it
  // acts on no object, so the pattern is an ordinary operand, and the factory
  // reinterprets the bits it spells rather than converting the number they
  // would otherwise stand for.
  if (b.method == support::BuiltinFn::kFromBits) {
    auto pattern = lower(RequiredOperands(c, 1).at(0));
    if (!pattern) return std::unexpected(std::move(pattern.error()));
    const mir::ExprId bits =
        block.exprs.Add(MakeToInt64Call(unit_lowerer.Unit(), *pattern));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = support::BuiltinFn::kFromBits},
                .arguments = {bits}},
        .type = result_type};
  }

  // The other direction acts on the real it reads out of, and answers in a
  // machine integer -- the fraction dropped, or the pattern itself -- which the
  // destination's declared representation then lands into, named as the type it
  // is.
  auto subject = lower(ObjectActedOn(b));
  if (!subject) return std::unexpected(std::move(subject.error()));
  const mir::ExprId read_out = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = b.method, .receiver = *subject},
                  .arguments = {}},
          .type = unit_lowerer.Unit().builtins.machine_int64});
  const mir::ExprId packed_type =
      mir::BuildPackedTypeRef(unit_lowerer.Unit(), block, result_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFromInt},
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
