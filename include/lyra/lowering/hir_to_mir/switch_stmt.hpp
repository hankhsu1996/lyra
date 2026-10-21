#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The statements one arm of a choice runs, lowered into the frame it is
// handed.
using ArmBody = std::function<diag::Result<mir::Block>(const WalkFrame&)>;

// One arm of a switch: the values it is taken at and what it runs.
struct ValueArm {
  std::vector<std::int64_t> values;
  ArmBody body;
};

// The values each item of a case is taken at, where every label of every item
// -- each lowered into the frame it is handed -- is a value of `type` known
// before the run with no x or z bit and `type` fits a machine integer: then a
// subject of `type` holding no x or z bit equals a label exactly where it reads
// as that label's integer. Absent at the first label that is not. Each label
// is lowered here into steps the statement never holds, only to ask what it
// is, so where the answer is absent every label is lowered again where the
// statement tries it.
[[nodiscard]] auto ValuesOfItems(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const std::vector<Evaluation>> labels, mir::TypeId type)
    -> diag::Result<std::optional<std::vector<std::vector<std::int64_t>>>>;

// The switch over `discriminant`, a machine integer and a node of `frame`'s
// block, with `default_body` running where no arm is taken. A value two arms
// name is the first one's, as the first match is the one a search in order
// takes.
auto BuildValueSwitch(
    const WalkFrame& frame, mir::ExprId discriminant,
    std::span<const ValueArm> arms, std::optional<mir::Block> default_body)
    -> diag::Result<mir::SwitchStmt>;

// The items of a case whose every label is a value known before the run,
// compared with the subject, the local `subject` of `subject_type`, as `case`
// compares (LRM 12.5): a switch over the subject's value, entered only where
// the subject holds no x or z bit, since such a subject equals no such label,
// in a block each item leaves by a break naming it, so `default_body` runs
// where no item is taken.
auto BuildCaseOverValues(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::LocalId subject, mir::TypeId subject_type,
    std::span<const ValueArm> items, std::optional<mir::Block> default_body)
    -> diag::Result<mir::BlockStmt>;

// Appends to `frame`'s block the steps that assign `held`, a machine boolean
// local that is false there, whether any of `conditions` holds: each is a step
// of its own, lowered where none before it held, so it is evaluated only there
// and the steps stand beside each other however many there are.
auto AssignAnyHoldsInOrder(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::LocalId held,
    std::span<const Evaluation> conditions) -> diag::Result<void>;

// One arm of a search: the conditions that select it, each tried in turn, and
// what it runs.
struct SearchedArm {
  std::vector<Evaluation> conditions;
  ArmBody body;
};

// The arms tried in order, the first one any of whose conditions holds running
// and leaving the rest untried, and `default_body` running where none does.
// The arms stand beside each other in a block a selected arm leaves by a break
// naming it, so a search of any number of arms is as deep as one of two. A
// condition is lowered where the arms before it, and the conditions before it
// in its own arm, have failed, so it is evaluated only there; and before the
// arm's statements, since a condition may introduce what they name.
auto BuildArmSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SearchedArm> arms, std::optional<mir::Block> default_body)
    -> diag::Result<mir::BlockStmt>;

}  // namespace lyra::lowering::hir_to_mir
