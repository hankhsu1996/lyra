#include "lyra/compiler/unit_metadata.hpp"

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata {
  const mir::Class& root = unit.GetClass(*unit.root);
  return ElaboratedUnitMetadata{
      .def_name = root.name,
      .time_precision_power = root.time_resolution.precision_power};
}

}  // namespace lyra::compiler
