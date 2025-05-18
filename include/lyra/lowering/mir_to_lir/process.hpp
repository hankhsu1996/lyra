#pragma once

namespace lyra::mir {
class Process;
}

namespace lyra::lir {
class Process;
}

namespace lyra::lowering::mir_to_lir {
class LirBuilder;

// Lowers a MIR Process into a LIR Process.
auto LowerProcess(const mir::Process& process, LirBuilder& builder) -> void;

}  // namespace lyra::lowering::mir_to_lir
