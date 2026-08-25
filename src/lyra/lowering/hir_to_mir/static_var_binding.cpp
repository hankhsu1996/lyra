#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"

#include <algorithm>
#include <format>
#include <vector>

#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

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
    UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    base::Arena<mir::FieldDecl, mir::FieldId>& fields, ObservedStorage observed,
    const hir::ProceduralBody& body,
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
      const mir::TypeId value_type = unit_lowerer.TranslateType(var.type);
      const mir::TypeId storage_type =
          observed == ObservedStorage::kYes
              ? unit_lowerer.Unit().types.ObservableCellOf(value_type)
              : value_type;
      bindings.push_back(
          StaticVarBinding{
              .var = var_id,
              .scope = scope_id,
              .field = fields.Add(
                  mir::FieldDecl{
                      .name = std::format(
                          "{}__{}_{}", callable_name, var.name, var_id.value),
                      .type = storage_type})});
    }
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      self_ref(self_ref, child);
    }
  };
  bind(bind, body.root_scope);
  return bindings;
}

}  // namespace lyra::lowering::hir_to_mir
