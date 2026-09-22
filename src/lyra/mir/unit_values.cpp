#include "lyra/mir/unit_values.hpp"

#include <utility>
#include <vector>

#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/integral_constant_materialization.hpp"
#include "lyra/mir/type_descriptor.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::mir {

void SettleUnitValues(CompilationUnit& unit) {
  std::vector<ValueBuild> constants;
  constants.reserve(unit.integral_constants.size());
  for (const IntegralConstantId id : unit.integral_constants.Ids()) {
    constants.push_back(MaterializeIntegralConstant(unit, id));
  }
  unit.builds.constants = {
      unit.integral_constants.size(), std::move(constants)};

  // The descriptions are reached only after the loop above, because that loop
  // is one of the things that adds to them. The count is taken again below
  // rather than reused from here, and that is what says nothing was added
  // while this one ran: a walk covers the entries there were when it started,
  // so a pool that ended larger has an answer missing and is refused rather
  // than half-built.
  std::vector<ValueBuild> descriptors;
  descriptors.reserve(unit.type_descriptors.size());
  for (const TypeDescriptorId id : unit.type_descriptors.Ids()) {
    descriptors.push_back(DescribeType(unit, id));
  }
  unit.builds.descriptors = {
      unit.type_descriptors.size(), std::move(descriptors)};
}

}  // namespace lyra::mir
