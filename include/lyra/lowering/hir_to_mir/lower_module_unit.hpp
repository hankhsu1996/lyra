#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower one HIR module unit into a MIR compilation unit. Returns the unit on
// success or the first user-facing diagnostic produced by the pass.
auto LowerModuleUnit(const hir::ModuleUnit& unit)
    -> diag::Result<mir::CompilationUnit>;

}  // namespace lyra::lowering::hir_to_mir
