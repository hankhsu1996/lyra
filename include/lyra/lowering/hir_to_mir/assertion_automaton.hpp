#pragma once

#include <cstdint>
#include <vector>

#include "lyra/hir/assertion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"

namespace lyra::lowering::hir_to_mir {

// A set of positions, in the machine words a generated transition reads it in.
// Every set of one automaton is the same width, and that width is what the
// storage holding the attempts is installed with.
using PositionSet = std::vector<std::uint64_t>;

// One position: a tick at which every one of these Boolean expressions holds.
// More than one conjunct is what a zero delay produces, which puts the end
// point of one sequence and the start point of the next at the same tick (LRM
// 16.7); none is the tick a delay measures across, which matches whatever the
// design does there.
struct AssertionPosition {
  std::vector<hir::ExprId> conjuncts;
};

// Where an evaluation of an implication's consequent starts, once a match of
// the antecedent ends at one of `antecedent_last` (LRM 16.12.7).
struct ImplicationSeed {
  PositionSet antecedent_last;
  PositionSet consequent_start;
  // `|->` starts the consequent at the end point of the match that triggered
  // it, `|=>` at the tick after.
  bool same_tick = false;
};

// The property as a finite automaton over positions, which is what one
// evaluation walks.
//
// The operator set the front end accepts has no unbounded window and admits no
// empty match, so this graph is acyclic and every position spans exactly one
// tick. That is what lets an evaluation be a set of positions and nothing
// besides: there is no counter to carry and no empty step to take.
struct AssertionAutomaton {
  std::vector<AssertionPosition> positions;
  // Where an evaluation live at a position is live next, given that position
  // held at this tick. Indexed by position.
  std::vector<PositionSet> follow;
  // Where an evaluation of the whole property begins.
  PositionSet start;
  // Positions at which a match completes something the attempt owed. A
  // sequence's matching is existential, so reaching one settles that
  // evaluation and nothing more is asked of it.
  PositionSet satisfying;
  // Positions belonging to an implication's antecedent. An evaluation of one
  // owes nothing, so running out of positions there is the implication being
  // true without its consequent read at all, rather than a failure (LRM
  // 16.12.7).
  PositionSet trigger;
  std::vector<ImplicationSeed> seeds;
  std::uint32_t words = 0;
};

// Builds it from the property tree the front end wrote, whose ids index the
// arenas of `body`.
[[nodiscard]] auto BuildAssertionAutomaton(
    const hir::ProceduralBody& body, const hir::PropertySpec& spec)
    -> AssertionAutomaton;

// The set naming one position, `words` wide. Which word a position falls in and
// which bit of it stands for the position is the automaton's own encoding, so
// everything that spells a set reaches it through here.
[[nodiscard]] auto PositionSetOf(std::uint32_t position, std::uint32_t words)
    -> PositionSet;

}  // namespace lyra::lowering::hir_to_mir
