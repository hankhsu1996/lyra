#pragma once

#include <span>

#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// The machine boolean `value`, appended to `block`.
[[nodiscard]] auto BuildMachineBool(
    const mir::CompilationUnit& unit, mir::Builder block, bool value)
    -> mir::ExprId;

// Reduces a lowered expression consumed as a condition to a stated boolean
// predicate, appending that predicate to `block` and returning its id. Every
// condition context (if / while / for / do-while / ternary) stores the reduced
// predicate, so a backend emits the reduction mechanically from the node and
// never re-derives it from the operand's value type (LRM 12.4: the condition is
// true when the expression is nonzero, and false when it is zero, x, or z). An
// expression that already is a predicate is its own reduction.
[[nodiscard]] auto ReduceToCondition(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId cond)
    -> mir::ExprId;

// The condition that holds exactly where `condition`, a reduced predicate, does
// not.
[[nodiscard]] auto BuildConditionNot(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId condition)
    -> mir::ExprId;

// Whether `value` holds as a condition, as a one-bit value: 1 where the
// condition is true and 0 where it is false, x or z, so never unknown.
[[nodiscard]] auto ConditionAsBit(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId;

// One arm of a chain of selections: the condition that selects it and what the
// chain answers where it does.
struct SelectedValue {
  mir::ExprId selected;
  mir::ExprId value;
};

// `s0 ? v0 : s1 ? v1 : ... : otherwise`, at `type`, appended to `block`. Each
// selection after the first stands as the third operand of the one before it,
// so the chain is as deep as it has arms, and one of no arms is `otherwise`.
[[nodiscard]] auto BuildSelectionChain(
    mir::Builder block, std::span<const SelectedValue> arms,
    mir::ExprId otherwise, mir::TypeId type) -> mir::ExprId;

// Whether every one of `conditions` holds, and whether any does, tested in
// order and each only when the ones before it have not already settled the
// answer. Each is read as a condition is read, and is tested only where it is
// reached; a condition whose evaluation takes steps of its own arrives with
// them in a block of its own, so they run only where it is tested. Nothing to
// test is the empty search, whose answer is the identity of the question.
// Each condition after the first stands as an operand of the one before it, so
// this is for the few conditions one construct composes; a list as long as
// the source makes it is a run of steps, each assigning the answer in turn.
[[nodiscard]] auto AllHold(
    const mir::CompilationUnit& unit, mir::Builder block,
    std::span<const mir::ExprId> conditions) -> mir::ExprId;
[[nodiscard]] auto AnyHolds(
    const mir::CompilationUnit& unit, mir::Builder block,
    std::span<const mir::ExprId> conditions) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
