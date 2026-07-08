#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::mir_to_lir {

// Lower one MIR compilation unit to a self-contained LIR unit. Every MIR type
// and identity is translated to a LIR-owned one; a construct not yet lowerable
// to LIR is rejected with a diagnostic rather than passed through.
auto LowerUnit(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit>;

}  // namespace lyra::lowering::mir_to_lir
