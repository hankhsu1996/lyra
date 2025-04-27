#pragma once

namespace lyra::mir {
class Module;
}

namespace lyra::lir {
class Module;
}

namespace lyra::lowering {

// Lowers a MIR Module into a LIR Module.
auto LowerModule(const mir::Module& module) -> lir::Module;

}  // namespace lyra::lowering
