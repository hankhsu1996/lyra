#include "lyra/lowering/hir_to_mir/endpoint.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

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
                    .fields.Get(field)
                    .type;
            return BoundEndpoint{
                .field = field,
                .field_type = field_type,
                .cell_type = field_type,
                .sealed = false};
          },
          [&](const hir::RoutedRef& c) -> BoundEndpoint {
            const auto& meta = lowerer.RoutedRefTarget(c.id);
            const auto& ptr =
                std::get<mir::PointerType>(unit.types.Get(meta.slot_type).data);
            return BoundEndpoint{
                .field = meta.target,
                .field_type = meta.slot_type,
                .cell_type = ptr.pointee,
                .sealed = true};
          },
      },
      route);
}

auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr {
  const mir::Expr field_access = BuildStructuralFieldAccessExpr(
      frame, unit, mir::EnclosingHops{0}, endpoint.field);
  if (!endpoint.sealed) {
    return field_access;
  }
  const mir::ExprId pointer = frame.current_block->exprs.Add(field_access);
  return mir::Expr{
      .data = mir::DerefExpr{.pointer = pointer}, .type = endpoint.cell_type};
}

auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId {
  const mir::ExprId field_access =
      block.exprs.Add(BuildStructuralFieldAccessExpr(
          frame, unit, mir::EnclosingHops{0}, endpoint.field));
  if (endpoint.sealed) {
    return field_access;
  }
  const mir::TypeId ptr_type = unit.types.PointerTo(
      endpoint.field_type, mir::PointerOwnership::kBorrowed);
  return block.exprs.Add(mir::MakeAddressOfExpr(field_access, ptr_type));
}

}  // namespace lyra::lowering::hir_to_mir
