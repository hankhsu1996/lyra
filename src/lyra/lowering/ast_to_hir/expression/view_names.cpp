#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"

#include <expected>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/published_modport.hpp"

namespace lyra::lowering::ast_to_hir {

auto PublishedViewNameOf(
    const UnitLowerer& unit_lowerer, hir::ScopeClassId scope_class,
    std::string_view modport, std::string_view name)
    -> hir::PublishedViewNameId {
  const std::optional<hir::PublishedViewNameId> published =
      unit_lowerer.Unit()
          .scope_classes.Get(scope_class)
          .FindViewName(modport, name);
  if (!published.has_value()) {
    throw InternalError(
        "PublishedViewNameOf: an interface publishes every name each of its "
        "views defines, and the front end has refused any other");
  }
  return *published;
}

auto PublishedViewNameType(
    const UnitLowerer& unit_lowerer, hir::ScopeClassId scope_class,
    hir::PublishedViewNameId name) -> hir::TypeId {
  const hir::ScopeClassSignature& scope =
      unit_lowerer.Unit().scope_classes.Get(scope_class);
  return std::visit(
      Overloaded{
          [](const hir::ViewDefinedPlace& place) { return place.type; },
          [&](const hir::ViewComputedValue& computed) {
            return scope.callables.Get(computed.evaluate).result_type;
          }},
      scope.ViewName(name));
}

auto LowerRoutedViewName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  // The view defining the name is the scope the port identifier is declared in
  // (LRM 25.5.4).
  const auto& selected =
      hve.symbol.getParentScope()->asSymbol().as<slang::ast::ModportSymbol>();
  // The object the view sits on is reached the way this unit reaches that
  // interface instance -- through the port a connection bound it to, upward
  // where the name's search landed above this instance, or down to an instance
  // declared here. Which of those is a fact about the object and not about the
  // name, so it is the same question a name reaching an ordinary member of that
  // instance asks.
  auto walked = unit_lowerer.RouteToScope(
      frame, *selected.getParentScope(),
      unit_lowerer.StartOf(frame, hve.symbol, hve.ref));
  if (!walked) return std::unexpected(std::move(walked.error()));
  std::optional<ScopeRoute> through = *std::move(walked);
  const auto* on = through.has_value()
                       ? std::get_if<InPublishedScope>(&through->place)
                       : nullptr;
  if (on == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a name a modport offers, reached this way, is not yet supported");
  }
  const hir::ScopeClassId scope_class = on->scope_class;
  const hir::PublishedViewNameId name = PublishedViewNameOf(
      unit_lowerer, scope_class, selected.name, hve.symbol.name);
  const hir::TypeId type =
      PublishedViewNameType(unit_lowerer, scope_class, name);
  return std::visit(
      Overloaded{
          [&](const hir::ViewDefinedPlace& place) {
            return hir::MakeRefExpr(
                unit_lowerer.MakeRoutedViewPlaceRef(
                    frame.Current(), *through, scope_class, name, place),
                type, span);
          },
          [&](const hir::ViewComputedValue&) {
            return hir::MakeRefExpr(
                unit_lowerer.MakeRoutedViewValueRef(
                    frame.Current(), *through, scope_class, name),
                type, span);
          }},
      unit_lowerer.Unit().scope_classes.Get(scope_class).ViewName(name));
}

}  // namespace lyra::lowering::ast_to_hir
