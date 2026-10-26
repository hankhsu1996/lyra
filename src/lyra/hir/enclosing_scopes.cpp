#include "lyra/hir/enclosing_scopes.hpp"

#include <map>
#include <optional>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

auto EnclosingScopesOf(const CompilationUnit& unit)
    -> std::vector<std::optional<BlockBodyId>> {
  std::map<support::DefPath, BlockBodyId> first_publishing;
  for (const BlockBodyId id : unit.block_bodies.Ids()) {
    first_publishing.try_emplace(
        unit.block_bodies.Get(id).scope.published.signature.class_path, id);
  }
  std::vector<std::optional<BlockBodyId>> enclosing;
  enclosing.reserve(unit.block_bodies.size());
  for (const BlockBodyId id : unit.block_bodies.Ids()) {
    support::DefPath around =
        unit.block_bodies.Get(id).scope.published.signature.class_path;
    if (around.data.empty()) {
      throw InternalError(
          "hir::EnclosingScopesOf: a block body publishes no class -- please "
          "report this as a bug");
    }
    around.data.pop_back();
    if (around.data.empty()) {
      enclosing.emplace_back();
      continue;
    }
    const auto found = first_publishing.find(around);
    if (found == first_publishing.end()) {
      throw InternalError(
          "hir::EnclosingScopesOf: a block body is published inside a class no "
          "scope of its unit publishes -- please report this as a bug");
    }
    enclosing.emplace_back(found->second);
  }
  return enclosing;
}

}  // namespace lyra::hir
