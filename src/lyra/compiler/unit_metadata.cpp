#include "lyra/compiler/unit_metadata.hpp"

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata {
  // Every scope of a unit runs at the unit's timescale, which the unit states
  // through the class its object tree is rooted at. A namespace unit (LRM 26)
  // roots none and so has no scope to give a timescale to.
  const mir::RootedTree* tree = mir::RootedTreeOf(unit);
  if (tree == nullptr) {
    return ElaboratedUnitMetadata{};
  }
  const mir::Class& root = unit.GetClass(tree->root);
  return ElaboratedUnitMetadata{.time_resolution = root.time_resolution};
}

}  // namespace lyra::compiler
