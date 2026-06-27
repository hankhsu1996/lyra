#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/method.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src) -> diag::Result<mir::MethodDecl>;

}  // namespace lyra::lowering::hir_to_mir
