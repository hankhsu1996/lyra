#include "lyra/mir_transform/expand_wide_integrals.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/wide_integral_operator.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/mir_transform/pass.hpp"

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
  // The call of the operation `source` over `operands`. An integral operator
  // takes every operand as an argument, so the call is entered on no object.
  auto Call(
      BlockRebuild& rebuild, const mir::TypeOperationSource& source,
      std::span<const mir::ExprId> operands, mir::TypeId answer) const
      -> mir::Expr {
    const std::optional<mir::CallableCode> declaration =
        mir::OperationDeclarationOf(*unit_, source);
    if (!declaration.has_value()) {
      throw InternalError(
          "mir_transform: a wide operator names an operation of a type that "
          "declares none such -- please report this as a bug");
    }
    std::vector<mir::ExprId> values;
    for (const mir::ExprId operand : operands) {
      values.push_back(rebuild.Expr(operand));
    }
    mir::PassedOperands passed = mir::PassedAsDeclared(
        *unit_, rebuild.Builder(), *declaration, std::nullopt,
        std::move(values));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target =
                            mir::TypeOperationTarget{
                                .type = source.type,
                                .answers = source.answers,
                                .arguments = source.arguments}},
                .arguments = std::move(passed.arguments)},
        .type = answer};
  }

  // The low part of `operand`, read at `type`.
  auto LowPart(BlockRebuild& rebuild, mir::ExprId operand, mir::TypeId type)
      const -> mir::Expr {
    const mir::TypeId position = mir::PositionType(unit_->types);
    const mir::ExprId value = rebuild.Expr(operand);
    mir::Builder block = rebuild.Builder();
    const mir::ExprId first = block.Add(
        mir::MakeIntegralLiteral(
            *unit_, position,
            mir::IntegralConstant{.value_words = {0}, .state_words = {}}));
    return mir::MakeCopyExpr(block.Slice(value, first, type), type);
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
