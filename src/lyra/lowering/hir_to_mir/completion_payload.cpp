#include "lyra/lowering/hir_to_mir/completion_payload.hpp"

#include <vector>

#include "lyra/hir/subroutine.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto CompletionComponentTypes(
    const ModuleLowerer& module, const hir::StructuralSubroutineDecl& decl)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> components;
  if (decl.result_var.has_value()) {
    components.push_back(module.TranslateType(decl.result_type));
  }
  for (const auto& param : decl.params) {
    if (param.direction == hir::ParamDirection::kOutput ||
        param.direction == hir::ParamDirection::kInOut) {
      components.push_back(
          module.TranslateType(decl.body.procedural_vars.Get(param.var).type));
    }
  }
  return components;
}

auto NormalizeCompletionPayload(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId {
  if (components.empty()) return unit.builtins.void_type;
  if (components.size() == 1) return components.front();
  return unit.types.Intern(mir::TupleType{.elements = components});
}

}  // namespace lyra::lowering::hir_to_mir
