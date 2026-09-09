#include "lyra/lowering/hir_to_mir/endpoint.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::ReferenceRoute& route) -> BoundEndpoint {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return std::visit(
      Overloaded{
          [&](const hir::DirectMemberRef& m) -> BoundEndpoint {
            const mir::FieldId field = lowerer.TranslateStructuralDataObject(
                hir::StructuralHops{0}, m.var);
            const mir::TypeId field_type =
                frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                    .cls->fields.Get(field)
                    .type;
            // A `ref` port's internal name owns no cell: it stands for the
            // connected variable's (LRM 23.3.3.2), and opening the reference is
            // what reaches that cell. Every other member is the cell.
            if (const auto* ref = unit.types.Get(field_type).As<mir::RefType>();
                ref != nullptr) {
              return BoundEndpoint{
                  .field = field,
                  .cell_type = mir::ObservableCellOf(unit.types, ref->pointee),
                  .reach = EndpointReach::kMemberIsAReference};
            }
            return BoundEndpoint{
                .field = field,
                .cell_type = field_type,
                .reach = EndpointReach::kMemberIsTheCell};
          },
          [&](const hir::RoutedRef& c) -> BoundEndpoint {
            const auto& meta = lowerer.RoutedRefTarget(c.id);
            const auto& ptr =
                unit.types.Get(meta.slot_type).Get<mir::PointerType>();
            return BoundEndpoint{
                .field = meta.target,
                .cell_type = ptr.pointee,
                .reach = EndpointReach::kMemberHoldsAPointer};
          },
      },
      route);
}

auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr {
  const mir::Expr field_access = BuildStructuralFieldAccessExpr(
      frame, unit, mir::EnclosingHops{0}, endpoint.field);
  switch (endpoint.reach) {
    // A reference answers for the operations on the cell it binds -- a read, a
    // write, a sampled read -- so naming it is naming that cell, and no step
    // stands between them here. Only a wait needs the cell as storage in its
    // own right, which is where the two part company.
    case EndpointReach::kMemberIsTheCell:
    case EndpointReach::kMemberIsAReference:
      return field_access;
    case EndpointReach::kMemberHoldsAPointer: {
      const mir::ExprId pointer = frame.current_block->exprs.Add(field_access);
      return mir::Expr{
          .data = mir::DerefExpr{.pointer = pointer},
          .type = endpoint.cell_type};
    }
  }
  throw InternalError("EndpointCellExpr: unknown endpoint reach");
}

auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId {
  const mir::ExprId field_access =
      block.exprs.Add(BuildStructuralFieldAccessExpr(
          frame, unit, mir::EnclosingHops{0}, endpoint.field));
  const auto address_of = [&](mir::ExprId cell) {
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = endpoint.cell_type,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kMutable}});
    return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
  };
  switch (endpoint.reach) {
    case EndpointReach::kMemberHoldsAPointer:
      return field_access;
    case EndpointReach::kMemberIsTheCell:
      return address_of(field_access);
    // What a wait registers on is the cell the reference binds, never the
    // reference itself, so the reference is opened before its address is taken.
    case EndpointReach::kMemberIsAReference:
      return address_of(block.exprs.Add(
          mir::Expr{
              .data = mir::DerefExpr{.pointer = field_access},
              .type = endpoint.cell_type}));
  }
  throw InternalError("EndpointObservablePtr: unknown endpoint reach");
}

}  // namespace lyra::lowering::hir_to_mir
