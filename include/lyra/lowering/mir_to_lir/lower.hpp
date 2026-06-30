#pragma once

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::mir_to_lir {

// Lower one MIR compilation unit to LIR. The returned unit borrows `unit` as
// its frozen type and metadata context, so `unit` must outlive the result.
auto LowerUnit(const mir::CompilationUnit& unit) -> lir::CompilationUnit;

}  // namespace lyra::lowering::mir_to_lir
