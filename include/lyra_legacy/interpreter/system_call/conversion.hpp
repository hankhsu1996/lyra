#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class InstructionContext;

/// Handle type conversion and bit cast functions.
/// Includes: $signed, $unsigned, $itor, $rtoi, $realtobits, $bitstoreal,
///           $shortrealtobits, $bitstoshortreal, $clog2
auto HandleConversionCalls(
    const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
