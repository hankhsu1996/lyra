#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A question put to an enumeration's member list about a value, and any
// operand the question takes after it.
auto AskMembers(
    UnitLowerer& unit_lowerer, mir::Block& block, support::BuiltinFn question,
    hir::TypeId enum_type, std::vector<mir::ExprId> operands,
    mir::TypeId answer_type) -> mir::Expr {
  const mir::ExprId members = mir::BuildEnumerationDescriptorRef(
      unit_lowerer.Unit(), block, unit_lowerer.TranslateType(enum_type));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = question, .receiver = members},
              .arguments = std::move(operands)},
      .type = answer_type};
}

// The member list an enumeration declares. It lives in the type pool, which
// interning a type grows, so a caller takes what it needs out of it before
// building anything.
auto MembersOf(UnitLowerer& unit_lowerer, hir::TypeId enum_type)
    -> const std::vector<mir::EnumMember>& {
  const mir::TypeId type = unit_lowerer.TranslateType(enum_type);
  const auto& enumeration =
      unit_lowerer.Unit().types.Get(type).Get<mir::EnumType>();
  if (enumeration.members.empty()) {
    throw InternalError(
        "an enumeration reached lowering with no members -- please report this "
        "as a bug");
  }
  return enumeration.members;
}

// The operand bearing the enumeration the called method belongs to: for an
// instance call it is the value the method is applied to, for a type-static one
// a bearer the source names the enumeration through.
auto BearerOf(const hir::CallExpr& c) -> hir::ExprId {
  if (c.arguments.empty() || !c.arguments.front().has_value()) {
    throw InternalError(
        "an enumerated type method reached lowering without the argument "
        "bearing its enumeration -- please report this as a bug");
  }
  return *c.arguments.front();
}

// LRM 6.19.5.3 `next` and 6.19.5.4 `prev`: the value, and how many members to
// step, which the source may omit and which then moves by one.
template <ExprLowerer Lowerer>
auto LowerStepCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    hir::ExprId bearer, hir::TypeId enum_type, support::BuiltinFn question,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto value_or = lowerer.LowerExpr(lowerer.HirExprs().Get(bearer), frame);
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  mir::Block& block = *frame.current_block;
  const mir::ExprId value = block.exprs.Add(*std::move(value_or));

  mir::ExprId count{};
  if (const std::optional<hir::ExprId> step = OptionalOperand(c, 1)) {
    auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(*step), frame);
    if (!count_or) return std::unexpected(std::move(count_or.error()));
    count = block.exprs.Add(*std::move(count_or));
  } else {
    const mir::CompilationUnit& unit = lowerer.Owner().Unit();
    count = BuildIntegralLiteral(
        unit, block, unit.builtins.int_unsigned,
        mir::IntegralConstant{.value_words = {1U}, .state_words = {}});
  }
  return AskMembers(
      lowerer.Owner(), block, question, enum_type, {value, count}, result_type);
}

}  // namespace

auto BuildEnumNameCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId value_id,
    hir::TypeId enum_type) -> mir::Expr {
  return AskMembers(
      unit_lowerer, block, support::BuiltinFn::kEnumerationName, enum_type,
      {value_id}, unit_lowerer.Unit().builtins.string);
}

auto BuildEnumMembershipCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId value_id,
    hir::TypeId enum_type) -> mir::Expr {
  return AskMembers(
      unit_lowerer, block, support::BuiltinFn::kEnumerationHas, enum_type,
      {value_id}, unit_lowerer.Unit().builtins.machine_int64);
}

template <ExprLowerer Lowerer>
auto LowerEnumMethod(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    hir::EnumMethodRef ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const hir::ExprId bearer = BearerOf(c);
  const hir::TypeId enum_type = lowerer.HirExprs().Get(bearer).type;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  switch (ref.method) {
    case hir::EnumMethod::kNum:
      return block.exprs.Get(BuildIntLiteral(
          unit, block,
          static_cast<std::int64_t>(
              MembersOf(lowerer.Owner(), enum_type).size())));
    case hir::EnumMethod::kFirst: {
      const mir::IntegralConstant first =
          MembersOf(lowerer.Owner(), enum_type).front().value;
      return block.exprs.Get(
          BuildIntegralLiteral(unit, block, result_type, first));
    }
    case hir::EnumMethod::kLast: {
      const mir::IntegralConstant last =
          MembersOf(lowerer.Owner(), enum_type).back().value;
      return block.exprs.Get(
          BuildIntegralLiteral(unit, block, result_type, last));
    }
    case hir::EnumMethod::kName: {
      auto value_or = lowerer.LowerExpr(lowerer.HirExprs().Get(bearer), frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      const mir::ExprId value_id = block.exprs.Add(*std::move(value_or));
      return BuildEnumNameCallExpr(lowerer.Owner(), block, value_id, enum_type);
    }
    case hir::EnumMethod::kNext:
      return LowerStepCall(
          lowerer, frame, c, bearer, enum_type,
          support::BuiltinFn::kEnumerationNext, result_type);
    case hir::EnumMethod::kPrev:
      return LowerStepCall(
          lowerer, frame, c, bearer, enum_type,
          support::BuiltinFn::kEnumerationPrev, result_type);
  }
  throw InternalError("LowerEnumMethod: unknown enumerated type method");
}

template auto LowerEnumMethod(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, hir::EnumMethodRef,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerEnumMethod(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    hir::EnumMethodRef, mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
