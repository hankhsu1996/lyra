#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class SystemCallContext;

/// Handle time-related system functions.
/// Includes: $time, $stime, $realtime, $timeformat, $timeunit, $timeunit_root,
///           $timeprecision, $timeprecision_root, $printtimescale,
///           $printtimescale_root
auto HandleTimeCalls(const lir::Instruction& instr, SystemCallContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
