#pragma once

#include "lyra/design_assembly/compiled_bindings.hpp"

namespace lyra::mir {
class Arena;
struct Design;
}  // namespace lyra::mir

namespace lyra::design_assembly {

void AssembleBindings(
    CompiledBindingPlan&& plan, mir::Arena& arena, mir::Design& design);

}  // namespace lyra::design_assembly
