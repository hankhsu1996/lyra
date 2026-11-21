#include "lyra/mir_transform/expand_wide_integrals.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/wide_integral_operator.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/integral_operation.hpp"

namespace lyra::mir_transform {

namespace {

class Expander {
 public:
  explicit Expander(const mir::CompilationUnit& unit) : unit_(&unit) {
  }

  auto Node(BlockRebuild& rebuild, mir::ExprId id) const
      -> std::optional<mir::Expr> {
    const mir::Block& from = rebuild.From();
    const mir::Expr& expr = from.exprs.Get(id);
    const std::optional<mir::IntegralOperator> applied =
        mir::IntegralOperatorOf(*unit_, from, expr);
    if (!applied.has_value()) {
      return std::nullopt;
    }
    const std::optional<mir::WideOperatorExpansion> expansion =
        mir::WideOperatorExpansionOf(*unit_, from, expr, *applied);
    if (!expansion.has_value()) {
      return std::nullopt;
    }
    return std::visit(
        Overloaded{
            // A conversion to a type no wider than a machine's integers is
            // the operand's low bits at that type (LRM 6.24.1), as a select of
            // them is.
            [&](const mir::Truncated&) {
              return LowPart(rebuild, applied->operands.front(), expr.type);
            },
            [&](const mir::CallsItsTypesOperation& call) {
              return Call(rebuild, call.source, applied->operands, expr.type);
            }},
        *expansion);
  }

 private:
  // The call of the operation `source` over `operands`, each passed as the
  // operation's declaration takes it: a value it reads by a reference where it
  // lies, or bound to the value where it is made, and a machine value as
  // itself.
  auto Call(
      BlockRebuild& rebuild, const mir::TypeOperationSource& source,
      std::span<const mir::ExprId> operands, mir::TypeId answer) const
      -> mir::Expr {
    const std::optional<mir::CallableCode> declaration =
        mir::OperationDeclarationOf(*unit_, source);
    std::vector<mir::ExprId> passed;
    for (std::size_t i = 0; i < operands.size(); ++i) {
      const mir::TypeId param_type =
          declaration->locals.Get(declaration->params.at(i)).type;
      const mir::ExprId value = rebuild.Expr(operands[i]);
      const auto* reference =
          unit_->types.Get(param_type).As<mir::ReferenceType>();
      passed.push_back(
          reference == nullptr ? value : Bound(rebuild, value, param_type));
    }
    mir::Direct direct{
        .target = mir::TypeOperationTarget{
            .type = source.type,
            .answers = source.answers,
            .arguments = source.arguments}};
    if (declaration->receiver.has_value()) {
      direct.receiver = passed.front();
      passed.erase(passed.begin());
    }
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = std::move(direct), .arguments = std::move(passed)},
        .type = answer};
  }

  // A read-only reference to `value`, a node of the block being built: to
  // the storage a read of it reads, and otherwise to the value itself, held to
  // the end of the full-expression.
  static auto Bound(
      BlockRebuild& rebuild, mir::ExprId value, mir::TypeId reference)
      -> mir::ExprId {
    mir::ExprId object = value;
    if (const auto* read =
            std::get_if<mir::CopyExpr>(&rebuild.To().exprs.Get(value).data);
        read != nullptr && !std::holds_alternative<mir::SliceExpr>(
                               rebuild.To().exprs.Get(read->place).data)) {
      object = read->place;
    }
    return rebuild.Add(
        mir::Expr{
            .data = mir::BorrowExpr{.operand = object}, .type = reference});
  }

  // The low part of `operand`, read at `type`.
  auto LowPart(BlockRebuild& rebuild, mir::ExprId operand, mir::TypeId type)
      const -> mir::Expr {
    const mir::TypeId position = mir::PositionType(unit_->types);
    const mir::ExprId first = rebuild.Add(
        mir::MakeIntegralLiteral(
            *unit_, position,
            mir::IntegralConstant{.value_words = {0}, .state_words = {}}));
    mir::ExprId receiver = rebuild.Expr(operand);
    if (const auto* read = std::get_if<mir::CopyExpr>(
            &rebuild.To().exprs.Get(receiver).data)) {
      receiver = read->place;
    }
    const mir::ExprId bits =
        rebuild.Add(mir::MakeSliceExpr(receiver, first, type));
    return mir::MakeCopyExpr(bits, type);
  }

  const mir::CompilationUnit* unit_;
};

// Whether any node of `block`, or of a block nested in it, is an operator a
// legalized unit states otherwise.
auto ExpandsAny(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  for (const mir::Expr& expr : block.exprs) {
    const std::optional<mir::IntegralOperator> applied =
        mir::IntegralOperatorOf(unit, block, expr);
    if (applied.has_value() &&
        mir::WideOperatorExpansionOf(unit, block, expr, *applied).has_value()) {
      return true;
    }
  }
  return std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
    return ExpandsAny(unit, child);
  });
}

}  // namespace

auto ExpandWideIntegralsOnBody(
    const PassContext& context, const mir::CallableCode& code)
    -> std::optional<mir::CallableCode> {
  if (!code.body.has_value() || !ExpandsAny(*context.unit, *code.body)) {
    return std::nullopt;
  }
  const Expander expander(*context.unit);
  const RebuildRules rules{.expr = [&](BlockRebuild& rebuild, mir::ExprId id) {
    return expander.Node(rebuild, id);
  }};
  return RebuildCode(code, rules);
}

}  // namespace lyra::mir_transform
