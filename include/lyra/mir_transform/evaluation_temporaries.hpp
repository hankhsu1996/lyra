#pragma once

#include <optional>
#include <set>
#include <utility>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::mir_transform {

// Two rewrites make the temporaries an expression's evaluation needs, each a
// local of the body declared by a step of the evaluation it belongs to and
// named where it is read, and each keeping the order the evaluation's nodes
// evaluate their operands in: an operand that computes something and is
// evaluated before one taking a step is taken into a temporary first, and a
// value a borrow refers to that the evaluation makes is made in one, since a
// step ends what it makes and the reference may be read after.

// Makes the evaluation order explicit, whatever size any value is: an operand
// read out of storage that a later operand of the same node may change or end
// -- a value the runtime holds rather than plain bits, or the handle the
// object an operation is entered on is reached through -- is taken into a
// temporary before that operand runs, so the node is handed what was read
// rather than what is there by the time it runs (LRM 8.29 reclaims only an
// object "no longer being used").
auto OrderEvaluation(mir::CompilationUnit unit) -> mir::CompilationUnit;

// Which values of one body its frame keeps apart, each held through a box:
// which of its locals, by their position among them, and which of the values
// its expressions make and read, each a node of the block named.
struct FramePlan {
  std::vector<bool> locals_apart;
  std::set<std::pair<const mir::Block*, mir::ExprId>> values_apart;
};

// `code`, every value `plan` keeps apart that an expression makes and another
// node reads given a temporary of its own, in the order the evaluation runs,
// which the frame keeps apart in turn and `plan` then names among the locals,
// and every such value a statement discards made in a box the statement
// discards with it; nothing where `code` makes no such value. Made where it is
// read, such a value would enclose its reader once it is held off the stack, so
// a chain the source writes flat would nest as deep as it is long.
auto GiveTemporariesToValuesKeptApart(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    FramePlan& plan) -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
