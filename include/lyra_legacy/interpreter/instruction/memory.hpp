#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}

namespace lyra::interpreter {

class InstructionContext;

/// Handle memory operations (load, store, packed bits, aggregates).
auto HandleMemoryOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
