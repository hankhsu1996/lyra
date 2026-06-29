#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"

namespace lyra::lowering::hir_to_mir {

// When the continuous-assignment target is a net, the process drives one of the
// net's driver slots instead of writing a cell: its body updates this driver
// handle member with the evaluated right-hand side (LRM 6.5).
struct NetDriver {
  mir::MemberId driver_member;
  mir::TypeId driver_type;
};

auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src,
    std::optional<NetDriver> net_driver = std::nullopt)
    -> diag::Result<mir::MethodDecl>;

}  // namespace lyra::lowering::hir_to_mir
