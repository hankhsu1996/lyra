#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/method.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerContinuousAssign(
    const ClassLowerer& lowerer, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src) -> diag::Result<mir::MethodDecl>;

}  // namespace lyra::lowering::hir_to_mir
