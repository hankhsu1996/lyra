#include "lyra/lowering/hir_to_mir/endpoint.hpp"

#include <format>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The member a route of parent edges ends at, reached on the object it climbed
// to -- the reader's own declaration where it climbs nothing, and otherwise the
// member the scope it climbed to published.
auto ClimbedEndpoint(
    const StructuralScopeLowerer& lowerer, const ClimbedRoute& climbed,
    const hir::DataLeaf& leaf) -> MemberPlace {
  // A declaration of the reader's own scope is named by its layout, which no
  // scope above it shares.
  const auto own = [&](const char* what) -> const StructuralScopeLowerer& {
    if (climbed.hops.value > 0) {
      throw InternalError(
          std::format(
              "ClimbedEndpoint: a route names {} of a scope above the reader's "
              "own, which it reaches only through what that scope published -- "
              "please report this as a bug",
              what));
    }
    return lowerer;
  };
  const mir::ClassFieldTarget field = std::visit(
      Overloaded{
          [&](const hir::StructuralDataObjectLeaf& l) {
            return own("a data object").TranslateStructuralDataObject(l.object);
          },
          [&](const hir::PublishedMemberLeaf& l) {
            const ScopeClassLayout& record =
                lowerer.Owner().ScopeClassLayoutOf(l.scope_class);
            return mir::ClassFieldTarget{
                .owner = record.cls,
                .slot = record.published.members.Get(l.member)};
          }},
      leaf);
  return MemberAtHops{
      .hops = mir::EnclosingHops{climbed.hops.value},
      .field = field,
      .member_type = FieldTypeOf(lowerer.Owner(), field)};
}

}  // namespace

auto WholeRoutedTarget(
    const UnitLowerer& owner, const hir::StructuralScope& scope,
    const hir::Expr& expr) -> std::optional<hir::RoutedValueRef> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  if (const auto* named = std::get_if<hir::RoutedValueRef>(&primary->data)) {
    return *named;
  }
  const auto* view = std::get_if<hir::RoutedViewPlaceRef>(&primary->data);
  if (view == nullptr) return std::nullopt;
  const hir::ViewPlaceRoute& route = scope.routes.view_places.Get(view->id);
  const hir::ScopeClassSignature& signature =
      owner.Hir().scope_classes.Get(route.scope_class);
  const auto& place =
      std::get<hir::ViewDefinedPlace>(signature.ViewName(route.name));
  if (!signature.IsOnePlace(place) || !place.parts.front().path.empty()) {
    return std::nullopt;
  }
  return route.parts.front();
}

auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> MemberPlace {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const RouteReach& reach = lowerer.ReachOf(reference.id);
  return std::visit(
      Overloaded{
          [&](const ClimbedRoute& climbed) -> MemberPlace {
            return ClimbedEndpoint(
                lowerer, climbed,
                lowerer.HirScope().routes.values.Get(reference.id).leaf);
          },
          // The slot is a pointer to the member, so what its type points at
          // is the member's.
          [&](const StoredRoute& stored) -> MemberPlace {
            const mir::TypeId slot_type =
                frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                    .cls->fields.Get(stored.slot)
                    .type;
            return MemberThroughSlot{
                .slot = stored.slot,
                .member_type =
                    unit.types.Get(slot_type).Get<mir::PointerType>().pointee};
          }},
      reach);
}

auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const MemberPlace& endpoint) -> mir::Expr {
  // A reference answers for the operations on the storage it binds -- a read,
  // a write, a sampled read, where a wait on it enrols -- so naming it is
  // naming that storage, and no step stands between them here.
  return std::visit(
      Overloaded{
          [&](const MemberAtHops& at) {
            return BuildStructuralFieldAccessExpr(
                frame, unit, at.hops, at.field, at.member_type);
          },
          [&](const MemberThroughSlot& through) {
            const mir::ExprId pointer =
                frame.Builder().Add(BuildStructuralFieldAccessExpr(
                    frame, unit, mir::EnclosingHops{0}, through.slot));
            return mir::MakeDerefExpr(
                frame.Builder().DerefOperand(unit.types, pointer),
                through.member_type);
          }},
      endpoint);
}

}  // namespace lyra::lowering::hir_to_mir
