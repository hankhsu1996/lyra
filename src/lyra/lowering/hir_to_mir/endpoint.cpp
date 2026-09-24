#include "lyra/lowering/hir_to_mir/endpoint.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The endpoint a member of type `member_type` gives wherever it is. A `ref`
// port's internal name owns no cell: it stands for the connected variable's
// (LRM 23.3.3.2), and opening the reference is what reaches that cell. Every
// other member is the cell.
auto EndpointAt(
    const mir::CompilationUnit& unit, MemberPlace place,
    mir::TypeId member_type) -> BoundEndpoint {
  if (const auto* ref = unit.types.Get(member_type).As<mir::RefType>();
      ref != nullptr) {
    return BoundEndpoint{
        .place = place,
        .kind = MemberKind::kReference,
        .cell_type = mir::ObservableCellOf(unit.types, ref->pointee)};
  }
  return BoundEndpoint{
      .place = place, .kind = MemberKind::kCell, .cell_type = member_type};
}

// The endpoint a route of parent edges gives: the member it ends at, reached
// in the class it climbed to. Such a route never leaves this unit, so it never
// ends at data another unit declares.
auto ClimbedEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const ClimbedRoute& climbed, const hir::DataLeaf& leaf) -> BoundEndpoint {
  const StructuralScopeLowerer& declaring =
      lowerer.EnclosingScopeAtHops(climbed.hops);
  const auto not_this_unit = []() -> mir::FieldId {
    throw InternalError(
        "ClimbedEndpoint: a route of parent edges stays in this unit, so it "
        "cannot end at data another unit declares");
  };
  const mir::FieldId field = std::visit(
      Overloaded{
          [&](const hir::StructuralDataObjectLeaf& l) {
            return declaring.TranslateStructuralDataObject(
                hir::StructuralHops{0}, l.object);
          },
          [&](const hir::ProceduralStaticLeaf& l) {
            return declaring.ProceduralStaticField(l.body, l.var);
          },
          [&](const hir::SignatureMemberLeaf&) { return not_this_unit(); },
          [&](const hir::OpaqueLeaf&) { return not_this_unit(); }},
      leaf);
  const mir::EnclosingHops hops{climbed.hops.value};
  return EndpointAt(
      lowerer.Owner().Unit(), MemberAtHops{.hops = hops, .field = field},
      frame.EnclosingClassAtHops(hops).cls->fields.Get(field).type);
}

// The member itself, as an lvalue: named where it sits, or reached by opening
// the slot that points at it. Appends any sub-expressions to the frame's block
// and returns the top expression unadded.
auto MemberExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const MemberPlace& place) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const MemberAtHops& at) {
            return BuildStructuralFieldAccessExpr(
                frame, unit, at.hops, at.field);
          },
          [&](const MemberThroughSlot& through) {
            const mir::ExprId pointer =
                frame.current_block->exprs.Add(BuildStructuralFieldAccessExpr(
                    frame, unit, mir::EnclosingHops{0}, through.slot));
            return mir::Expr{
                .data = mir::DerefExpr{.pointer = pointer},
                .type = through.member_type};
          }},
      place);
}

}  // namespace

auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return std::visit(
      Overloaded{
          [&](const ClimbedRoute& climbed) -> BoundEndpoint {
            return ClimbedEndpoint(
                lowerer, frame, climbed,
                lowerer.HirScope().routes.values.Get(reference.id).leaf);
          },
          // The slot is a pointer to the member, so what its type points at
          // is the member's.
          [&](const StoredRoute& stored) -> BoundEndpoint {
            const mir::TypeId slot_type =
                frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                    .cls->fields.Get(stored.slot)
                    .type;
            const mir::TypeId member_type =
                unit.types.Get(slot_type).Get<mir::PointerType>().pointee;
            return EndpointAt(
                unit,
                MemberThroughSlot{
                    .slot = stored.slot, .member_type = member_type},
                member_type);
          }},
      lowerer.ReachOf(reference.id));
}

auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr {
  // A reference answers for the operations on the cell it binds -- a read, a
  // write, a sampled read -- so naming it is naming that cell, and no step
  // stands between them here. Only a wait needs the cell as storage in its
  // own right, which is where the two part company.
  return MemberExpr(frame, unit, endpoint.place);
}

auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId {
  const auto address_of = [&](mir::ExprId cell) {
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = endpoint.cell_type,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kMutable}});
    return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
  };
  const WalkFrame at_block = frame.WithBlock(&block);
  switch (endpoint.kind) {
    case MemberKind::kCell:
      return std::visit(
          Overloaded{
              [&](const MemberAtHops&) {
                return address_of(block.exprs.Add(
                    MemberExpr(at_block, unit, endpoint.place)));
              },
              // The slot already holds the cell's address.
              [&](const MemberThroughSlot& through) {
                return block.exprs.Add(BuildStructuralFieldAccessExpr(
                    at_block, unit, mir::EnclosingHops{0}, through.slot));
              }},
          endpoint.place);
    // What a wait registers on is the cell the reference binds, never the
    // reference itself, so the reference is opened before its address is taken.
    case MemberKind::kReference:
      return address_of(block.exprs.Add(
          mir::Expr{
              .data =
                  mir::DerefExpr{
                      .pointer = block.exprs.Add(
                          MemberExpr(at_block, unit, endpoint.place))},
              .type = endpoint.cell_type}));
  }
  throw InternalError("EndpointObservablePtr: unknown member kind");
}

}  // namespace lyra::lowering::hir_to_mir
