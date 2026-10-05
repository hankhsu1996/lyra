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
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The endpoint a member gives wherever it is. A `ref` port's internal name owns
// no cell: it stands for the connected variable's storage (LRM 23.3.3.2). Every
// other member is the cell.
auto EndpointAt(const mir::CompilationUnit& unit, MemberPlace place)
    -> BoundEndpoint {
  const mir::TypeId member_type = std::visit(
      Overloaded{
          [](const MemberAtHops& at) { return at.member_type; },
          [](const MemberThroughSlot& through) { return through.member_type; }},
      place);
  return BoundEndpoint{
      .place = place,
      .kind = unit.types.Get(member_type).Is<mir::RefType>()
                  ? MemberKind::kReference
                  : MemberKind::kCell};
}

// The endpoint a route of parent edges gives: the member it ends at, reached
// on the object it climbed to. Such a route never leaves this unit, so it
// never ends at data another unit declares.
auto ClimbedEndpoint(
    const StructuralScopeLowerer& lowerer, const ClimbedRoute& climbed,
    const hir::DataLeaf& leaf) -> BoundEndpoint {
  const StructuralScopeLowerer& declaring =
      lowerer.EnclosingScopeAtHops(climbed.hops);
  const auto not_this_unit = []() -> mir::ClassFieldTarget {
    throw InternalError(
        "ClimbedEndpoint: a route of parent edges stays in this unit, so it "
        "cannot end at data another unit declares");
  };
  const mir::ClassFieldTarget field = std::visit(
      Overloaded{
          [&](const hir::StructuralDataObjectLeaf& l) {
            return declaring.TranslateStructuralDataObject(
                hir::StructuralHops{0}, l.object);
          },
          [&](const hir::ProceduralStaticLeaf& l) {
            return declaring.ProceduralStaticField(l.body, l.var);
          },
          [&](const hir::ExternalMemberLeaf&) { return not_this_unit(); }},
      leaf);
  return EndpointAt(
      lowerer.Owner().Unit(),
      MemberAtHops{
          .hops = mir::EnclosingHops{climbed.hops.value},
          .field = field,
          .member_type = FieldTypeOf(lowerer.Owner(), field)});
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
                frame, unit, at.hops, at.field, at.member_type);
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
                lowerer, climbed,
                lowerer.HirScope().routes.values.Get(reference.id).leaf);
          },
          // The slot is a pointer to the member, so what its type points at
          // is the member's.
          [&](const StoredRoute& stored) -> BoundEndpoint {
            const mir::TypeId slot_type =
                frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                    .cls->fields.Get(stored.slot)
                    .type;
            return EndpointAt(
                unit, MemberThroughSlot{
                          .slot = stored.slot,
                          .member_type = unit.types.Get(slot_type)
                                             .Get<mir::PointerType>()
                                             .pointee});
          }},
      lowerer.ReachOf(reference.id));
}

auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr {
  // A reference answers for the operations on the storage it binds -- a read,
  // a write, a sampled read -- so naming it is naming that storage, and no step
  // stands between them here. Only a wait asks something else of it, what a
  // write through it is told to, which is where the two part company.
  return MemberExpr(frame, unit, endpoint.place);
}

auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId {
  const auto address_of = [&](mir::ExprId cell) {
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = block.exprs.Get(cell).type,
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
    case MemberKind::kReference:
      return ReferenceReportsTo(
          unit, block,
          block.exprs.Add(MemberExpr(at_block, unit, endpoint.place)));
  }
  throw InternalError("EndpointObservablePtr: unknown member kind");
}

auto ReferenceReportsTo(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId reference)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kReferenceReportsTo},
                  .arguments = {reference}},
          .type = mir::ErasedPointer(unit.types)});
}

}  // namespace lyra::lowering::hir_to_mir
