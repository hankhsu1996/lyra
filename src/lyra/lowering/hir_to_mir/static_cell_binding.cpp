#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"

#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/assertion_control.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The cell the scope already placed for `var` where it published that local,
// and otherwise a fresh one in `owner`.
auto PlacedOrDeclared(
    const StaticStorageOwner& owner, hir::ProceduralVarId var,
    mir::TypeId cell_type) -> StaticStorageHome {
  if (const auto* instance = std::get_if<InstanceStorage>(&owner)) {
    for (const PlacedStatic& placed : instance->placed) {
      if (placed.var == var) return InstanceFieldHome{.field = placed.field};
    }
  }
  return DeclareStaticCell(owner, cell_type);
}

}  // namespace

auto DeclareStaticCell(const StaticStorageOwner& owner, mir::TypeId cell_type)
    -> StaticStorageHome {
  return std::visit(
      Overloaded{
          [&](const InstanceStorage& instance) -> StaticStorageHome {
            return InstanceFieldHome{
                .field = mir::ClassFieldTarget{
                    .owner = instance.owner,
                    .slot = instance.shape->AddField(cell_type)}};
          },
          [&](const ClassStorage& cls) -> StaticStorageHome {
            return ClassCellHome{
                .owner = cls.owner,
                .property = cls.shape->static_properties.Add(
                    mir::StaticPropertyDecl{.type = cell_type})};
          },
          [&](const UnitStorage& unit) -> StaticStorageHome {
            return UnitCellHome{
                .variable = unit.variables->Add(
                    mir::StaticVariableDecl{
                        .type = cell_type, .defining_unit = std::nullopt})};
          }},
      owner);
}

auto BindBodyStatics(
    const UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const StaticStorageOwner& owner, const hir::ProceduralBody& body)
    -> StaticCellBindings {
  StaticCellBindings bindings;
  const mir::TypeId control_type = mir::LibraryTypeOf(
      unit_lowerer.Unit().types, support::LibraryDecl::kAssertionControl);
  const mir::TypeId attempts_type = mir::LibraryTypeOf(
      unit_lowerer.Unit().types, support::LibraryDecl::kEvaluationAttempts);
  // Descends the body's lexical scope tree, since a declaration states its
  // scope only by sitting in it, carrying the scopes passed on the way down.
  std::vector<hir::ProceduralScopeId> enclosing;
  const auto bind = [&](const auto& self_ref,
                        hir::ProceduralScopeId scope_id) -> void {
    const hir::ProceduralScopeDecl& scope = scopes.Get(scope_id);
    enclosing.push_back(scope_id);
    for (const hir::ProceduralVarId var_id : scope.declarations) {
      const hir::ProceduralVarDecl& var = body.procedural_vars.Get(var_id);
      if (var.lifetime != hir::VariableLifetime::kStatic) continue;
      // One variable serves every activation of the body, whatever process each
      // runs in (LRM 6.21), so one process may write it while another waits on
      // it (LRM 9.4.2).
      const mir::TypeId cell_type = mir::ObservableCellOf(
          unit_lowerer.Unit().types, unit_lowerer.TranslateType(var.type));
      bindings.push_back(
          StaticCellBinding{
              .of = var_id,
              .scope = scope_id,
              .home = PlacedOrDeclared(owner, var_id, cell_type),
              .cell_type = cell_type});
    }
    for (const hir::ControlledAssertionId assertion :
         scope.controlled_assertions) {
      bindings.push_back(
          StaticCellBinding{
              .of =
                  AssertionControlCell{
                      .assertion = assertion, .within = enclosing},
              .scope = scope_id,
              .home = DeclareStaticCell(owner, control_type),
              .cell_type = control_type});
      if (support::HasEvaluationAttempts(
              body.controlled_assertions.Get(assertion).type)) {
        bindings.push_back(
            StaticCellBinding{
                .of = ConcurrentAttemptsCell{.assertion = assertion},
                .scope = scope_id,
                .home = DeclareStaticCell(owner, attempts_type),
                .cell_type = attempts_type});
      }
    }
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      self_ref(self_ref, child);
    }
    enclosing.pop_back();
  };
  bind(bind, body.root_scope);
  return bindings;
}

auto InstanceFieldOf(const StaticCellBinding& binding)
    -> mir::ClassFieldTarget {
  const auto* home = std::get_if<InstanceFieldHome>(&binding.home);
  if (home == nullptr) {
    throw InternalError(
        "InstanceFieldOf: a static-lifetime cell of a body outside the design "
        "hierarchy was asked for its field, but no object holds one -- please "
        "report this as a bug");
  }
  return home->field;
}

auto BuildStaticStorageAccess(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const StaticStorageHome& home, mir::TypeId cell_type,
    mir::EnclosingHops hops) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const InstanceFieldHome& instance) {
            return BuildStructuralFieldAccessExpr(
                frame, unit, hops, instance.field, cell_type);
          },
          [&](const ClassCellHome& cls) {
            return mir::Expr{
                .data =
                    mir::ReferenceExpr{
                        .target =
                            mir::StaticPropertyRef{
                                .owner = cls.owner, .prop = cls.property}},
                .type = cell_type};
          },
          [&](const UnitCellHome& namespace_cell) {
            return mir::Expr{
                .data =
                    mir::ReferenceExpr{
                        .target =
                            mir::StaticVariableRef{
                                .variable = namespace_cell.variable}},
                .type = cell_type};
          }},
      home);
}

}  // namespace lyra::lowering::hir_to_mir
