#pragma once

#include <cstdint>
#include <optional>
#include <variant>
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
// 16.7); none is a tick that matches whatever the design does there -- one a
// delay measures across, the tick after a nonoverlapped antecedent's match, a
// sequence's match kept once it is made, or where a selection reads its
// condition.
struct AssertionPosition {
  std::vector<hir::ExprId> conjuncts;
};

// What an evaluation answers, as Annex F.5.3.2's four answers in their order,
// computed from the positions it is live at and what its implications'
// instances answered. Each term names earlier terms of the same part by their
// index there, so a part's answer is its last term and computing the terms in
// order computes it.
//
// A sequence matched at some tick holds strongly from then on; one with a path
// still live holds where it is weak, since the trace so far has not shown it
// unable to match, and is pending where it is strong; one with no path left
// fails (LRM 16.12.2).
struct SequenceTerm {
  PositionSet live;
  PositionSet kept;
  hir::SequenceStrength strength;
};

// The order reversed (LRM 16.12.3).
struct NegatedTerm {
  std::uint32_t operand;
};

// A conjunction (LRM 16.12.5).
struct LeastTerm {
  std::uint32_t lhs;
  std::uint32_t rhs;
};

// A disjunction (LRM 16.12.4).
struct GreatestTerm {
  std::uint32_t lhs;
  std::uint32_t rhs;
};

// `r |-> p` holds where `p` holds after every match of `r` (Annex F.5.3.1), so
// this is the least answer of the instances `implication` started, one per
// match. An antecedent still live may match again, so it holds without holding
// strongly. Annex F.3.4.3 writes `|=>`, the followed-by operators, and the
// if-else and case properties in terms of `|->`, so each of them is numbered
// as an implication too.
struct ImplicationTerm {
  PositionSet antecedent;
  std::uint32_t implication;
};

using AnswerTerm = std::variant<
    SequenceTerm, NegatedTerm, LeastTerm, GreatestTerm, ImplicationTerm>;

// A part of the property an evaluation answers for: the whole of it, or what
// an implication asks of each match of its antecedent, or an arm a selection
// takes. Each instance of a part is an evaluation of its own: an implication's
// instances are questions about different suffixes of the trace (LRM 16.12.7),
// and which arm a selection takes is known only once its condition is read.
struct AutomatonPart {
  PositionSet start;
  std::vector<AnswerTerm> terms;
};

// Each match of the antecedent starts an instance of `part` at the tick the
// match ends (LRM 16.12.7, 16.12.9). For `|=>` and `#=#` the antecedent is the
// antecedent one tick longer, which is how Annex F.3.4.3.3 defines them, so
// every instance starts at the tick of the match that seeded it.
struct MatchSeed {
  std::uint32_t part;
};

// LRM 16.12.6: the condition, read where the antecedent matched, starts one arm
// at that tick, and with no else arm a false condition starts none.
struct ConditionSeed {
  hir::ExprId condition;
  std::uint32_t then_part;
  std::optional<std::uint32_t> else_part;
};

struct CaseSeedItem {
  std::vector<hir::ExprId> labels;
  std::uint32_t part;
};

// LRM 16.12.16: the subject, read where the antecedent matched, starts the
// first item whose label it matches as `case` compares (LRM 12.5), otherwise
// the default, otherwise none.
struct CaseSeed {
  hir::ExprId subject;
  std::vector<CaseSeedItem> items;
  std::optional<std::uint32_t> default_part;
};

// What starts the instances of one implication, at each tick one of the
// positions in `matches` matches. Seeds are listed outermost first, so one
// forward pass over them follows every seed a tick starts, however deep they
// nest.
struct ImplicationSeed {
  PositionSet matches;
  std::uint32_t implication;
  std::variant<MatchSeed, ConditionSeed, CaseSeed> starts;
};

// The property as a finite automaton over positions, which is what one
// evaluation walks, together with the code that answers each part.
//
// The operator set the front end accepts has no unbounded window and admits no
// empty match, so the graph a sequence spans is acyclic and every position
// spans exactly one tick. That is what lets an evaluation be a set of positions
// and nothing besides: there is no counter to carry and no empty step to take.
// The one cycle is a kept match, which stays where it is.
struct AssertionAutomaton {
  std::vector<AssertionPosition> positions;
  // Where an evaluation live at a position is live next, given that position
  // held at this tick. Indexed by position.
  std::vector<PositionSet> follow;
  // The first part is the whole property.
  std::vector<AutomatonPart> parts;
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
