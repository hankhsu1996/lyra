#include "lyra/mir_transform/elaborate_handle_derefs.hpp"

#include <algorithm>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/object_operand.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir_transform {

namespace {

auto OpensAHandle(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return mir::HandleReached(unit.types, block, expr).has_value();
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return OpensAHandle(unit, child);
         });
}

auto WithHandlesOpened(
    const mir::CompilationUnit& unit, const mir::CallableCode& code)
    -> mir::CallableCode {
  const RebuildRules rules{
      .expr = [&](BlockRebuild& rebuild,
                  mir::ExprId id) -> std::optional<mir::Expr> {
        const mir::Expr& expr = rebuild.From().exprs.Get(id);
        const std::optional<mir::ObjectOperand> reached =
            mir::HandleReached(unit.types, rebuild.From(), expr);
        if (!reached.has_value()) {
          return std::nullopt;
        }
        // The object the handle names, as the class it is viewed as; the
        // library answers where it lies.
        const mir::TypeId viewed_as = std::visit(
            Overloaded{
                [&](support::TheClassReached) {
                  return unit.types
                      .Get(rebuild.From().exprs.Get(reached->operand).type)
                      .Get<mir::ManagedRefType>()
                      .pointee;
                },
                [&](support::LibraryDecl library_class) {
                  return mir::LibraryTypeOf(unit.types, library_class);
                }},
            reached->view);
        return mir::Expr{
            .data = rebuild.WithOperandsTaken(
                expr.data,
                [&](mir::ExprId operand) {
                  const mir::ExprId taken = rebuild.Expr(operand);
                  return operand == reached->operand
                             ? rebuild.Add(
                                   mir::MakeObjectOfCall(
                                       unit, taken, viewed_as))
                             : taken;
                }),
            .type = expr.type};
      }};
  return RebuildCode(code, rules);
}

}  // namespace

auto ElaborateHandleDerefsOnBody(
    const PassContext& context, const mir::CodeSource&,
    const mir::CallableCode& code) -> std::optional<mir::CallableCode> {
  const mir::CompilationUnit& unit = *context.unit;
  if (!code.body.has_value() || !OpensAHandle(unit, *code.body)) {
    return std::nullopt;
  }
  return WithHandlesOpened(unit, code);
}

}  // namespace lyra::mir_transform
