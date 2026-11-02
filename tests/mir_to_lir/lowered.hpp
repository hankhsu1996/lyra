#pragma once

#include <expected>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::mir_to_lir {

// `unit` lowered with no target asking anything of it, which is all a test of
// this pass reads.
inline auto Lowered(const mir::CompilationUnit& unit)
    -> diag::Result<lir::CompilationUnit> {
  auto lowerer = UnitLowerer::LowerUnit(unit);
  if (!lowerer) {
    return std::unexpected(std::move(lowerer.error()));
  }
  lowerer->StateTypeSteps();
  return std::move(*lowerer).Finish();
}

}  // namespace lyra::lowering::mir_to_lir
