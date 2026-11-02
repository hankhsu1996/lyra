#include "lyra/lowering/hir_to_mir/expression/view_names.hpp"

#include <concepts>
#include <cstddef>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/concatenation.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto SignatureOf(const UnitLowerer& unit_lowerer, const ViewNameUse& use)
    -> const hir::ScopeClassSignature& {
  return unit_lowerer.Hir().scope_classes.Get(use.scope_class);
}

// The storage `use` stands for, or nothing where it is a value.
auto PlaceOf(const UnitLowerer& unit_lowerer, const ViewNameUse& use)
    -> const hir::ViewDefinedPlace* {
  return std::get_if<hir::ViewDefinedPlace>(
      &SignatureOf(unit_lowerer, use).ViewName(use.name));
}

// The scope whose routes a body of `lowerer` reads.
template <ExprLowerer L>
auto ScopeLowererOf(const L& lowerer) -> const StructuralScopeLowerer& {
  if constexpr (std::same_as<L, StructuralScopeLowerer>) {
    return lowerer;
  } else {
    return lowerer.EnclosingScopeLowerer();
  }
}

// Hands `visit` each part of `place` with the place of the member it descends
// from, in the instance `use` reaches, the first part first.
template <ExprLowerer L, typename Visit>
auto ForEachPartMember(
    L& lowerer, const WalkFrame& frame, const ViewNameUse& use,
    const hir::ViewDefinedPlace& place, Visit visit) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const hir::ViewPlaceRoute* route) -> diag::Result<void> {
            for (std::size_t i = 0; i < place.parts.size(); ++i) {
              visit(
                  LowerRoutedValueRefExpr(
                      ScopeLowererOf(lowerer), frame, route->parts[i]),
                  place.parts[i]);
            }
            return {};
          },
          [](const hir::ViewValueRoute*) -> diag::Result<void> {
            throw InternalError(
                "ForEachPartMember: a route to a value has no parts");
          },
          [&](const hir::InterfaceInstanceAccessExpr* held)
              -> diag::Result<void> {
            auto object = HeldInterfaceObject(lowerer, frame, *held);
            if (!object) return std::unexpected(std::move(object.error()));
            const mir::ExprId reached = EvaluatedOnce(frame, *object);
            for (const hir::MemberProjection& part : place.parts) {
              visit(
                  HeldMemberPlace(
                      lowerer.Owner(), frame, reached, use.scope_class,
                      part.member),
                  part);
            }
            return {};
          }},
      use.reached);
}

// The value one part holds: what its member shows, then each selector of its
// path as a read takes it.
auto PartValue(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Expr member,
    const hir::MemberProjection& part, hir::TypeId member_type) -> mir::Expr {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  // A member of an instance is no automatic variable of a body, so its cell
  // keeps a sampled value (LRM 16.5.1).
  mir::Expr value = PlaceContents(unit, frame, std::move(member), true);
  hir::TypeId receiver_type = member_type;
  for (const hir::PublishedSelector& selector : part.path) {
    const DescentStep step =
        PublishedSelectorStep(unit_lowerer, block, selector, receiver_type);
    value = StepRead(unit, block, step, block.Add(std::move(value)));
    receiver_type = hir::ProjectedTypeOf(selector);
  }
  return value;
}

template <ExprLowerer L>
auto LowerPlaceValue(
    L& lowerer, const WalkFrame& frame, const ViewNameUse& use,
    const hir::ViewDefinedPlace& place, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::ScopeClassSignature& scope = SignatureOf(unit_lowerer, use);
  std::vector<mir::Expr> values;
  values.reserve(place.parts.size());
  auto reached = ForEachPartMember(
      lowerer, frame, use, place,
      [&](mir::Expr member, const hir::MemberProjection& part) {
        values.push_back(PartValue(
            unit_lowerer, frame, std::move(member), part,
            scope.members.Get(part.member).type));
      });
  if (!reached) return std::unexpected(std::move(reached.error()));
  if (scope.IsOnePlace(place)) return std::move(values.front());
  mir::Builder block = frame.Builder();
  std::vector<mir::ExprId> operands;
  operands.reserve(values.size());
  for (mir::Expr& value : values) {
    operands.push_back(block.AsValue(unit.types, block.Add(std::move(value))));
  }
  return BuildValueConversion(
      unit, block, BuildBitsJoin(unit, frame, operands), result_type);
}

// The call a computed name stands for, on the instance `use` reaches.
template <ExprLowerer L>
auto LowerComputedValue(
    L& lowerer, const WalkFrame& frame, const ViewNameUse& use,
    const hir::ViewComputedValue& computed, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const hir::UnitObjectReceiver receiver = std::visit(
      Overloaded{
          [](const hir::ViewValueRoute* route) -> hir::UnitObjectReceiver {
            return route->object;
          },
          [](const hir::ViewPlaceRoute*) -> hir::UnitObjectReceiver {
            throw InternalError(
                "LowerComputedValue: a route to storage reaches no object");
          },
          [](const hir::InterfaceInstanceAccessExpr* held)
              -> hir::UnitObjectReceiver { return *held; }},
      use.reached);
  return LowerHirCallExpr(
      lowerer, frame,
      hir::CallExpr{
          .callee =
              hir::PublishedScopeMethodRef{
                  .receiver = receiver,
                  .scope_class = use.scope_class,
                  .callable = computed.evaluate},
          .arguments = {}},
      frame.span, result_type);
}

}  // namespace

template <ExprLowerer L>
auto ViewNameUseOf(const L& lowerer, const hir::RoutedViewPlaceRef& reference)
    -> ViewNameUse {
  const hir::ViewPlaceRoute& route =
      ScopeLowererOf(lowerer).HirScope().routes.view_places.Get(reference.id);
  return ViewNameUse{
      .reached = &route, .scope_class = route.scope_class, .name = route.name};
}

template <ExprLowerer L>
auto ViewNameUseOf(const L& lowerer, const hir::RoutedViewValueRef& reference)
    -> ViewNameUse {
  const hir::ViewValueRoute& route =
      ScopeLowererOf(lowerer).HirScope().routes.view_values.Get(reference.id);
  return ViewNameUse{
      .reached = &route, .scope_class = route.scope_class, .name = route.name};
}

template <ExprLowerer L>
auto ViewNameUseOf(const L& lowerer, const hir::Expr& expr)
    -> std::optional<ViewNameUse> {
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data)) {
    if (const auto* place =
            std::get_if<hir::RoutedViewPlaceRef>(&primary->data)) {
      return ViewNameUseOf(lowerer, *place);
    }
    if (const auto* value =
            std::get_if<hir::RoutedViewValueRef>(&primary->data)) {
      return ViewNameUseOf(lowerer, *value);
    }
    return std::nullopt;
  }
  const auto* held = std::get_if<hir::InterfaceMemberAccessExpr>(&expr.data);
  if (held == nullptr) return std::nullopt;
  const auto* name = std::get_if<hir::PublishedViewNameId>(&held->component);
  if (name == nullptr) return std::nullopt;
  return ViewNameUse{
      .reached = &held->instance,
      .scope_class = held->scope_class,
      .name = *name};
}

auto IsOneViewPlace(const UnitLowerer& unit_lowerer, const ViewNameUse& use)
    -> bool {
  const hir::ViewDefinedPlace* place = PlaceOf(unit_lowerer, use);
  return place != nullptr && SignatureOf(unit_lowerer, use).IsOnePlace(*place);
}

auto IsViewJoin(const UnitLowerer& unit_lowerer, const ViewNameUse& use)
    -> bool {
  const hir::ViewDefinedPlace* place = PlaceOf(unit_lowerer, use);
  return place != nullptr && !SignatureOf(unit_lowerer, use).IsOnePlace(*place);
}

template <ExprLowerer L>
auto LowerViewNameValue(
    L& lowerer, WalkFrame frame, const ViewNameUse& use,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::ViewDefinedPlace& place) {
            return LowerPlaceValue(lowerer, frame, use, place, result_type);
          },
          [&](const hir::ViewComputedValue& computed) {
            return LowerComputedValue(
                lowerer, frame, use, computed, result_type);
          }},
      SignatureOf(lowerer.Owner(), use).ViewName(use.name));
}

template <ExprLowerer L>
auto LowerViewNameParts(L& lowerer, WalkFrame frame, const ViewNameUse& use)
    -> diag::Result<std::vector<ViewPart>> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::ScopeClassSignature& scope = SignatureOf(unit_lowerer, use);
  const hir::ViewDefinedPlace* place = PlaceOf(unit_lowerer, use);
  if (place == nullptr) {
    throw InternalError(
        "LowerViewNameParts: a name a view offers only for reading is a value, "
        "and the front end refuses a write to it");
  }
  mir::Builder block = frame.Builder();
  std::vector<ViewPart> parts;
  parts.reserve(place->parts.size());
  auto reached = ForEachPartMember(
      lowerer, frame, use, *place,
      [&](mir::Expr member, const hir::MemberProjection& part) {
        const hir::TypeId member_type = scope.members.Get(part.member).type;
        AccessPath path{.owner = block.Add(std::move(member)), .descent = {}};
        hir::TypeId receiver_type = member_type;
        for (const hir::PublishedSelector& selector : part.path) {
          path = DescendInto(
              std::move(path),
              PublishedSelectorStep(
                  unit_lowerer, block, selector, receiver_type));
          receiver_type = hir::ProjectedTypeOf(selector);
        }
        parts.push_back(
            ViewPart{
                .place = std::move(path),
                .type = part.TypeWithin(member_type)});
      });
  if (!reached) return std::unexpected(std::move(reached.error()));
  return parts;
}

template <ExprLowerer L>
auto LowerViewNamePlace(L& lowerer, WalkFrame frame, const ViewNameUse& use)
    -> diag::Result<AccessPath> {
  if (!IsOneViewPlace(lowerer.Owner(), use)) {
    return diag::Fail(
        frame.span, diag::DiagCode::kUnsupportedExpressionForm,
        "naming one place of a name a modport joins or computes is not yet "
        "supported");
  }
  auto parts = LowerViewNameParts(lowerer, frame, use);
  if (!parts) return std::unexpected(std::move(parts.error()));
  return std::move(parts->front().place);
}

template auto ViewNameUseOf(
    const ProcessLowerer&, const hir::RoutedViewPlaceRef&) -> ViewNameUse;
template auto ViewNameUseOf(
    const StructuralScopeLowerer&, const hir::RoutedViewPlaceRef&)
    -> ViewNameUse;
template auto ViewNameUseOf(
    const ProcessLowerer&, const hir::RoutedViewValueRef&) -> ViewNameUse;
template auto ViewNameUseOf(
    const StructuralScopeLowerer&, const hir::RoutedViewValueRef&)
    -> ViewNameUse;
template auto ViewNameUseOf(const ProcessLowerer&, const hir::Expr&)
    -> std::optional<ViewNameUse>;
template auto ViewNameUseOf(const StructuralScopeLowerer&, const hir::Expr&)
    -> std::optional<ViewNameUse>;
template auto LowerViewNameValue(
    ProcessLowerer&, WalkFrame, const ViewNameUse&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerViewNameValue(
    const StructuralScopeLowerer&, WalkFrame, const ViewNameUse&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerViewNameParts(ProcessLowerer&, WalkFrame, const ViewNameUse&)
    -> diag::Result<std::vector<ViewPart>>;
template auto LowerViewNameParts(
    const StructuralScopeLowerer&, WalkFrame, const ViewNameUse&)
    -> diag::Result<std::vector<ViewPart>>;
template auto LowerViewNamePlace(ProcessLowerer&, WalkFrame, const ViewNameUse&)
    -> diag::Result<AccessPath>;
template auto LowerViewNamePlace(
    const StructuralScopeLowerer&, WalkFrame, const ViewNameUse&)
    -> diag::Result<AccessPath>;

}  // namespace lyra::lowering::hir_to_mir
