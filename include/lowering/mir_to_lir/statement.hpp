#pragma once

namespace lyra::mir {
class Statement;
}

namespace lyra::lowering {
class LirBuilder;

// Lowers a MIR Statement into LIR instructions.
auto LowerStatement(const mir::Statement& statement, LirBuilder& builder)
    -> void;

}  // namespace lyra::lowering
