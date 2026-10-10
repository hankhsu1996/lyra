#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <array>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether `left` matches the single value `value` (LRM 11.4.13): by wildcard
// equality where the comparison is of integral values, so an x or z bit of
// `value` is a do-not-care and one of `left` is not, and by equality where it
// is not. The front end brings the left operand and every member that is no
// array to the type the comparison is made at; an element of an array is
// reached only while the program runs, so it is brought there here.
auto BuildMatch(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId left,
    mir::ExprId value) -> mir::ExprId {
  const mir::TypeId compared_at = block.exprs.Get(left).type;
  const mir::Type& type = unit.types.Get(compared_at);
  const bool integral = type.IsIntegral();
  if (integral || type.IsRealFamily() || type.Is<mir::StringType>()) {
    value = ConvertToType(unit, block, value, compared_at);
  }
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block,
      integral ? hir::BinaryOp::kWildcardEquality : hir::BinaryOp::kEquality,
      left, value,
      OneBitAnswerType(
          unit, std::array{compared_at, block.exprs.Get(value).type})));
}

// Whether `left` matches any single value `value` holds, both already in the
// frame's block. An unpacked array holds the single values its elements hold
// (LRM 11.4.13), and how many elements it has is known only while the program
// runs, so the answer over them is the array's own OR reduction (LRM 7.12.3)
// of this same question asked of each element: one bit, unknown where no
// element matched and some comparison was unknown, and 0 for an array holding
// nothing.
auto BuildMatchAnywhereIn(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId left,
    mir::ExprId value) -> mir::ExprId {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId held = block.exprs.Get(value).type;
  const std::optional<mir::TypeId> element =
      unit.types.Get(held).ContainerElementType();
  if (!element.has_value()) {
    return BuildMatch(unit, block, left, value);
  }

  // The reduction hands each element and its index (LRM 7.12.4) to the
  // question, which reads the left operand as it was when the set was reached.
  ClosureBuilder each(unit_lowerer.Unit(), frame);
  const mir::LocalId item = each.AddParamAnonymous(*element);
  each.AddParamAnonymous(ArrayMethodIndexType(unit, held));
  const mir::ExprId left_in_each =
      SnapshotIntoClosure(unit_lowerer, frame, each, left);
  const mir::ExprId matched = BuildMatchAnywhereIn(
      unit_lowerer, each.Frame(), left_in_each,
      each.Body().exprs.Add(mir::MakeLocalRefExpr(item, *element)));
  const mir::TypeId answer = each.Body().exprs.Get(matched).type;
  const mir::ExprId question = block.exprs.Add(each.Build(matched));
  return block.exprs.Add(BuildBuiltinMethodCall(
      unit_lowerer, block, support::BuiltinFn::kOr, value, {question}, answer));
}

}  // namespace

template <ExprLowerer Lowerer>
auto BuildSetMemberTest(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId left, hir::ExprId member,
    mir::TypeId result_type) -> diag::Result<mir::ExprId> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  auto& unit = lowerer.Owner().Unit();
  const auto lower = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(hir_exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };

  const auto* range =
      std::get_if<hir::ValueRangeExpr>(&hir_exprs.Get(member).data);
  if (range == nullptr) {
    auto value = lower(member);
    if (!value) return std::unexpected(std::move(value.error()));
    return ConvertToType(
        unit, block, BuildMatchAnywhereIn(lowerer.Owner(), frame, left, *value),
        result_type);
  }

  // A range holds what lies between its bounds, each included, and a bound the
  // source left open excludes nothing on its side (LRM 11.4.13).
  std::vector<mir::TypeId> compared{block.exprs.Get(left).type};
  const auto bound = [&](const std::optional<hir::ExprId>& written)
      -> diag::Result<std::optional<mir::ExprId>> {
    if (!written.has_value()) return std::nullopt;
    auto value = lower(*written);
    if (!value) return std::unexpected(std::move(value.error()));
    compared.push_back(block.exprs.Get(*value).type);
    return *value;
  };
  auto low = bound(range->lo);
  if (!low) return std::unexpected(std::move(low.error()));
  auto high = bound(range->hi);
  if (!high) return std::unexpected(std::move(high.error()));

  const mir::TypeId type = OneBitAnswerType(unit, compared);
  std::vector<mir::ExprId> tests;
  const auto admits = [&](hir::BinaryOp relation,
                          const std::optional<mir::ExprId>& limit) {
    if (!limit.has_value()) return;
    tests.push_back(block.exprs.Add(
        BuildMirBinaryExpr(unit, block, relation, left, *limit, type)));
  };
  admits(hir::BinaryOp::kGreaterEqual, *low);
  admits(hir::BinaryOp::kLessEqual, *high);
  return ConvertToType(
      unit, block, BuildMirLogicalAnd(unit, block, type, tests), result_type);
}

template auto BuildSetMemberTest(
    ProcessLowerer&, WalkFrame, mir::ExprId, hir::ExprId, mir::TypeId)
    -> diag::Result<mir::ExprId>;
template auto BuildSetMemberTest(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId, hir::ExprId,
    mir::TypeId) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
