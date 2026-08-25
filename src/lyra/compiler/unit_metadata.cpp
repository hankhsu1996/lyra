#include "lyra/compiler/unit_metadata.hpp"

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata {
  // Every scope of a unit runs at the unit's precision, which the unit states
  // through the class its object tree is rooted at. A namespace unit (LRM 26)
  // roots none and so has no scope to give a precision to.
  if (!unit.root.has_value()) {
    return ElaboratedUnitMetadata{};
  }
  const mir::Class& root = unit.GetClass(*unit.root);
  return ElaboratedUnitMetadata{
      .time_precision_power = root.time_resolution.precision_power};
}

}  // namespace lyra::compiler
