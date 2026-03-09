#pragma once

#include "lyra/mir/compiled_bindings.hpp"

namespace lyra::mir {
class Arena;
struct Design;
}  // namespace lyra::mir

namespace lyra::realization {

void AssembleBindings(
    mir::CompiledBindingPlan&& plan, mir::Arena& arena, mir::Design& design);

}  // namespace lyra::realization
