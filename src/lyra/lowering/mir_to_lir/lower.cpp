#include "lyra/lowering/mir_to_lir/lower.hpp"

#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

namespace lyra::lowering::mir_to_lir {

auto LowerUnit(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  return UnitLowerer(unit).Run();
}

}  // namespace lyra::lowering::mir_to_lir
