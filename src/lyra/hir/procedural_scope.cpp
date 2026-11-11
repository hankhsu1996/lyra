#include "lyra/hir/procedural_scope.hpp"

#include <vector>

#include "lyra/base/registry.hpp"

namespace lyra::hir {

auto RootProceduralScopes(
    const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes)
    -> std::vector<ProceduralScopeId> {
  std::vector<bool> held(scopes.size(), false);
  for (const ProceduralScopeDecl& scope : scopes) {
    for (const ProceduralScopeId child : scope.child_scopes) {
      held[child.value] = true;
    }
  }
  std::vector<ProceduralScopeId> roots;
  for (const ProceduralScopeId id : scopes.Ids()) {
    if (!held[id.value]) roots.push_back(id);
  }
  return roots;
}

}  // namespace lyra::hir
