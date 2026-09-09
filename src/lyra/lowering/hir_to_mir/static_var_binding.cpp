#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"

#include <algorithm>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The type the cell takes, which is decided by whether anything outside the
// declaring body can reach it. Storage on an object of the design hierarchy can
// be reached by a hierarchical reference and waited on (LRM 6.21, 9.4.2), and
// so can one the unit's namespace owns (LRM 26.2), so both are observable
// cells. A cell a class owns is reached by no name outside the body -- a class
// is reached by member select rather than by scope name (LRM 23.7) -- so it is
// a plain one.
auto CellTypeFor(
    const UnitLowerer& unit_lowerer, const StaticStorageOwner& owner,
    mir::TypeId value_type) -> mir::TypeId {
  return std::holds_alternative<ClassStorage>(owner)
             ? value_type
             : mir::ObservableCellOf(unit_lowerer.Unit().types, value_type);
}

}  // namespace

auto DeclareStaticCell(
    const StaticStorageOwner& owner, std::string name, mir::TypeId cell_type)
    -> StaticStorageHome {
  return std::visit(
      Overloaded{
          [&](const InstanceStorage& instance) -> StaticStorageHome {
            return InstanceFieldHome{
                .field = instance.fields->Add(
                    mir::FieldDecl{
                        .name = std::move(name), .type = cell_type})};
          },
          [&](const ClassStorage& cls) -> StaticStorageHome {
            return ClassCellHome{
                .property = cls.properties->Add(
                    mir::StaticPropertyDecl{
                        .name = std::move(name), .type = cell_type})};
          },
          [&](const UnitStorage& unit) -> StaticStorageHome {
            unit.variables->Add(
                mir::StaticVariableDecl{.name = name, .type = cell_type});
            return UnitCellHome{.name = std::move(name)};
          }},
      owner);
}

auto SignatureBoundVars(const hir::SubroutineDecl& decl)
    -> std::vector<hir::ProceduralVarId> {
  std::vector<hir::ProceduralVarId> bound;
  bound.reserve(decl.params.size() + 1);
  for (const hir::SubroutineParam& param : decl.params) {
    bound.push_back(param.var);
  }
  if (decl.result_var.has_value()) {
    bound.push_back(*decl.result_var);
  }
  return bound;
}

auto BindBodyStatics(
    const UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const StaticStorageOwner& owner, const hir::ProceduralBody& body,
    std::span<const hir::ProceduralVarId> signature_bound,
    std::string_view callable_name) -> StaticVarBindings {
  StaticVarBindings bindings;
  // Descends the body's lexical scope tree, since a declaration states its
  // scope only by sitting in it.
  const auto bind = [&](const auto& self_ref,
                        hir::ProceduralScopeId scope_id) -> void {
    const hir::ProceduralScopeDecl& scope = scopes.Get(scope_id);
    for (const hir::ProceduralVarId var_id : scope.declarations) {
      const hir::ProceduralVarDecl& var = body.procedural_vars.Get(var_id);
      if (var.lifetime != hir::VariableLifetime::kStatic) continue;
      if (std::ranges::contains(signature_bound, var_id)) continue;
      const mir::TypeId cell_type = CellTypeFor(
          unit_lowerer, owner, unit_lowerer.TranslateType(var.type));
      bindings.push_back(
          StaticVarBinding{
              .var = var_id,
              .scope = scope_id,
              .home = DeclareStaticCell(
                  owner,
                  std::format(
                      "{}__{}_{}", callable_name, var.name, var_id.value),
                  cell_type),
              .cell_type = cell_type});
    }
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      self_ref(self_ref, child);
    }
  };
  bind(bind, body.root_scope);
  return bindings;
}

auto InstanceFieldOf(const StaticVarBinding& binding) -> mir::FieldId {
  const auto* home = std::get_if<InstanceFieldHome>(&binding.home);
  if (home == nullptr) {
    throw InternalError(
        "InstanceFieldOf: a static-lifetime local of a body outside the design "
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
                frame, unit, hops, instance.field);
          },
          [&](const ClassCellHome& cls) {
            return mir::Expr{
                .data =
                    mir::ReferenceExpr{
                        .target =
                            mir::StaticPropertyRef{
                                .owner = frame.current_class_id,
                                .prop = cls.property}},
                .type = cell_type};
          },
          [&](const UnitCellHome& namespace_cell) {
            return mir::Expr{
                .data =
                    mir::ReferenceExpr{
                        .target =
                            mir::ExternalUnitVariableRef{
                                .unit_name = unit.name,
                                .variable_name = namespace_cell.name}},
                .type = cell_type};
          }},
      home);
}

}  // namespace lyra::lowering::hir_to_mir
