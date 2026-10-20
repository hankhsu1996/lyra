#pragma once

#include <cstddef>
#include <functional>
#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// What one arm of a series contributes, lowered into the frame it is handed:
// whether the arm is taken, and the statements it runs.
using ArmTest =
    std::function<diag::Result<mir::ExprId>(const WalkFrame&, std::size_t)>;
using ArmBody =
    std::function<diag::Result<mir::Block>(const WalkFrame&, std::size_t)>;

// A series of arms tried in order, the first whose test holds ending it and
// `fall_through` running where none does:
//
//   if (test 0) body 0 else { if (test 1) body 1 else { ... fall_through } }
//
// Each arm is the `else` of the one before it, and an `else` holds its `if` and
// nothing beside it, so a target that writes such an `else` as `else if` opens
// nothing per arm; a test that took a step of its own would stand beside the
// `if` and break that. The arms are still each inside the one before, so
// whatever walks the series recurses once per arm. An arm's test is lowered
// before its statements, since a test may introduce what the statements name.
// Answers with the statement the series is, which stands in `frame`'s block:
// the first arm, or with no arms what falls through, as `case (x) default: ...`
// is.
auto BuildArmChain(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::size_t arm_count, const ArmTest& test_of, const ArmBody& body_of,
    std::optional<mir::Block> fall_through) -> diag::Result<mir::StmtData>;

}  // namespace lyra::lowering::hir_to_mir
