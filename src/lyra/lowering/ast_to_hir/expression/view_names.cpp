#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"

#include <expected>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/ast_to_hir/published_projection.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A member the interface published, on the instance the name is reached on.
auto MemberOnInstance(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const ViewNameOnInstance& view_name, hir::PublishedMemberId id,
    diag::SourceSpan span) -> hir::Expr {
  const hir::PublishedMember member =
      unit_lowerer.Unit()
          .external_unit_objects.Get(view_name.object)
          .members.Get(id);
  return std::visit(
      Overloaded{
          [&](const ScopeRoute& route) {
            return unit_lowerer.MakeRoutedMemberRef(
                frame.Current(),
                hir::ValueRoute{
                    .head = route.head,
                    .steps = route.steps,
                    .leaf =
                        hir::SignatureMemberLeaf{
                            .object = view_name.object,
                            .member = id,
                            .storage = member.storage,
                            .type = member.type}},
                span);
          },
          [&](const hir::InterfaceInstanceAccessExpr& held) {
            return hir::Expr{
                .type = member.type,
                .data =
                    hir::InterfaceMemberAccessExpr{
                        .instance = held, .member = id},
                .span = span};
          }},
      view_name.instance);
}

// The instance the name is reached on, as the object a call is made on.
auto InstanceAsReceiver(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const ViewNameOnInstance& view_name) -> hir::UnitObjectReceiver {
  return std::visit(
      Overloaded{
          [&](const ScopeRoute& route) -> hir::UnitObjectReceiver {
            const hir::TypeId object_type = unit_lowerer.Unit().types.Intern(
                hir::Type{hir::UnitObjectType{
                    .unit_name =
                        unit_lowerer.Unit()
                            .external_unit_objects.Get(view_name.object)
                            .unit_name}});
            return unit_lowerer.MakeRoutedObjectRef(
                frame.Current(), route, object_type);
          },
          [](const hir::InterfaceInstanceAccessExpr& held)
              -> hir::UnitObjectReceiver { return held; }},
      view_name.instance);
}

// The storage a name a view defines designates, as a place this unit reaches:
// each part reached as the member the interface published and descended by the
// path it stated, then joined. A concatenation of places is itself a place (LRM
// 11.4.12), so what comes back is written, read, driven and taken over exactly
// as any other place is.
auto ViewDefinedPlaceExpr(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const ViewNameOnInstance& view_name, const hir::ViewDefinedPlace& place,
    diag::SourceSpan span) -> hir::Expr {
  std::vector<hir::Expr> parts;
  parts.reserve(place.parts.size());
  for (const hir::MemberProjection& part : place.parts) {
    parts.push_back(ProjectPublishedPath(
        unit_lowerer, frame, part.path,
        MemberOnInstance(unit_lowerer, frame, view_name, part.member, span),
        span));
  }
  // Joining is what gives the name a type its parts do not have, so a single
  // part already carrying the name's type is the name -- and one that does not
  // was joined by the view and is joined here too.
  if (parts.size() == 1 && parts.front().type == place.type) {
    return std::move(parts.front());
  }
  std::vector<hir::ExprId> operands;
  operands.reserve(parts.size());
  for (hir::Expr& part : parts) {
    operands.push_back(frame.Exprs().Add(std::move(part)));
  }
  return hir::Expr{
      .type = place.type,
      .data = hir::ConcatExpr{.operands = std::move(operands)},
      .span = span};
}

auto ResolveRoutedViewName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<ViewNameOnInstance> {
  // The view defining the name is the scope the port identifier is declared in
  // (LRM 25.5.4).
  const auto& selected =
      hve.symbol.getParentScope()->asSymbol().as<slang::ast::ModportSymbol>();
  // The object the view sits on is reached the way this unit reaches that
  // interface instance -- through the port a connection bound it to, or by a
  // route down to an instance the design declares inside this unit. Which of
  // the two is a fact about the object and not about the name, so it is the
  // same question a name reaching an ordinary member of that instance asks.
  auto through =
      hve.ref.isViaIfacePort()
          ? unit_lowerer.ReachOneThroughInterfacePort(frame, hve.ref)
          : unit_lowerer.RouteToScope(frame, *selected.getParentScope());
  if (!through.has_value() || !through->unit_name.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a name a view offers, reached past what the interface promised, is "
        "not yet supported");
  }
  const hir::ExternalUnitObjectId object =
      unit_lowerer.ExternalUnitObjectOf(*through->unit_name);
  return ViewNameOnInstance{
      .instance = *std::move(through),
      .object = object,
      .meaning = PublishedViewNameMeaning(
          unit_lowerer, object, selected.name, hve.symbol.name)};
}

}  // namespace

auto PublishedViewNameMeaning(
    const UnitLowerer& unit_lowerer, hir::ExternalUnitObjectId object,
    std::string_view modport, std::string_view name) -> hir::ViewDefinedName {
  const hir::PublishedModport* view = hir::FindModport(
      unit_lowerer.Unit().external_unit_objects.Get(object).modports, modport);
  const hir::PublishedModportPort* published =
      view == nullptr ? nullptr : view->Find(name);
  if (published == nullptr) {
    throw InternalError(
        "PublishedViewNameMeaning: an interface publishes every name each of "
        "its views defines, and the front end has refused any other");
  }
  return published->meaning;
}

auto LowerViewNameOnInstance(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const ViewNameOnInstance& view_name, diag::SourceSpan span) -> hir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::ViewDefinedPlace& place) -> hir::Expr {
            return ViewDefinedPlaceExpr(
                unit_lowerer, frame, view_name, place, span);
          },
          [&](const hir::ViewComputedValue& computed) -> hir::Expr {
            hir::UnitObjectReceiver receiver =
                InstanceAsReceiver(unit_lowerer, frame, view_name);
            return hir::Expr{
                .type = unit_lowerer.Unit()
                            .external_unit_objects.Get(view_name.object)
                            .callables.Get(computed.evaluate)
                            .result_type,
                .data =
                    hir::CallExpr{
                        .callee =
                            hir::ExternalUnitMethodRef{
                                .receiver = std::move(receiver),
                                .object = view_name.object,
                                .callable = computed.evaluate},
                        .arguments = {}},
                .span = span};
          }},
      view_name.meaning);
}

auto LowerRoutedViewName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto resolved = ResolveRoutedViewName(unit_lowerer, frame, hve, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  return LowerViewNameOnInstance(unit_lowerer, frame, *resolved, span);
}

}  // namespace lyra::lowering::ast_to_hir
