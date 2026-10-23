#include "lyra/lowering/hir_to_mir/assertion_automaton.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

using Positions = std::vector<std::uint32_t>;

void AddPosition(Positions& into, std::uint32_t position) {
  if (std::ranges::find(into, position) == into.end()) {
    into.push_back(position);
  }
}

void AddPositions(Positions& into, const Positions& from) {
  for (const std::uint32_t position : from) {
    AddPosition(into, position);
  }
}

void RemovePosition(Positions& from, std::uint32_t position) {
  const auto found = std::ranges::find(from, position);
  if (found != from.end()) {
    from.erase(found);
  }
}

[[nodiscard]] auto Holds(const Positions& set, std::uint32_t position) -> bool {
  return std::ranges::find(set, position) != set.end();
}

// Which word of a position set a position falls in, and which bit of that word
// stands for it. Every set an automaton hands out is spelled through here. A
// set is spelled before the automaton's width is known, so it grows to reach
// the position and is widened to the automaton's width once the whole property
// is built.
void SetPosition(PositionSet& set, std::uint32_t position) {
  if (set.size() <= position / 64) {
    set.resize((position / 64) + 1, 0);
  }
  set[position / 64] |= std::uint64_t{1} << (position % 64);
}

auto Compose(const Positions& positions) -> PositionSet {
  PositionSet set;
  for (const std::uint32_t position : positions) {
    SetPosition(set, position);
  }
  return set;
}

// Every position from `first` up to but not including `end`.
auto PositionsFrom(std::uint32_t first, std::size_t end) -> Positions {
  Positions range;
  for (auto position = first; position < end; ++position) {
    range.push_back(position);
  }
  return range;
}

// The automaton as it is built: positions carry indices, which is what a set
// operation reads, and the machine words a transition reads them in are
// composed from them.
class Builder {
 public:
  explicit Builder(const hir::ProceduralBody& body) : body_(&body) {
  }

  [[nodiscard]] auto Build(const hir::PropertySpec& spec) -> AssertionAutomaton;

 private:
  // A sequence as its two boundaries: where a match of it may begin, and where
  // one may end. Everything between is already in the follow relation.
  struct SequenceFragment {
    Positions first;
    Positions last;
  };

  // A property as where an evaluation of it begins and the term answering it.
  struct PropertyFragment {
    Positions start;
    std::uint32_t term;
  };

  // A part whose terms are still to be built, from a property that is negated
  // where it is the consequent of a followed-by: `s #-# p` asks of some match
  // what `not (s |-> not p)` asks (LRM 16.12.9).
  struct UnbuiltPart {
    hir::PropertyExprId property;
    bool negated;
    std::uint32_t index;
  };

  [[nodiscard]] auto NewPosition(std::vector<hir::ExprId> conjuncts)
      -> std::uint32_t;
  auto NewPart(hir::PropertyExprId property, bool negated) -> std::uint32_t;
  [[nodiscard]] auto NewImplication() -> std::uint32_t;
  [[nodiscard]] auto BuildSequence(hir::SequenceExprId id) -> SequenceFragment;
  [[nodiscard]] auto BuildDelay(const hir::SequenceDelay& delay)
      -> SequenceFragment;
  [[nodiscard]] auto BuildRepetition(const hir::SequenceRepetition& repetition)
      -> SequenceFragment;
  void BuildPart(const UnbuiltPart& unbuilt);
  [[nodiscard]] auto BuildAnswer(
      hir::PropertyExprId id, std::vector<AnswerTerm>& terms)
      -> PropertyFragment;
  [[nodiscard]] auto BuildSeeded(
      hir::SequenceExprId antecedent, hir::PropertyExprId consequent,
      hir::ConsequentStart start, bool negated, std::vector<AnswerTerm>& terms)
      -> PropertyFragment;
  [[nodiscard]] auto BuildSelection(
      std::variant<MatchSeed, ConditionSeed, CaseSeed> starts,
      std::vector<AnswerTerm>& terms) -> PropertyFragment;
  void Connect(const Positions& from, const Positions& to);
  [[nodiscard]] auto Fuse(
      const SequenceFragment& head, const SequenceFragment& tail)
      -> SequenceFragment;
  void Unreach(std::uint32_t position);

  const hir::ProceduralBody* body_;
  std::vector<AssertionPosition> positions_;
  std::vector<Positions> follow_;
  std::vector<AutomatonPart> parts_;
  std::deque<UnbuiltPart> unbuilt_;
  std::vector<ImplicationSeed> seeds_;
  std::uint32_t implications_ = 0;
};

auto Builder::NewPosition(std::vector<hir::ExprId> conjuncts) -> std::uint32_t {
  const auto index = static_cast<std::uint32_t>(positions_.size());
  positions_.push_back(AssertionPosition{.conjuncts = std::move(conjuncts)});
  follow_.emplace_back();
  return index;
}

void Builder::Connect(const Positions& from, const Positions& to) {
  for (const std::uint32_t source : from) {
    AddPositions(follow_[source], to);
  }
}

// A position nothing enters any more: the fused copies below stand where it
// stood, and leaving it reachable would make an evaluation look live at a tick
// where no path of the sequence survives.
void Builder::Unreach(std::uint32_t position) {
  for (Positions& targets : follow_) {
    RemovePosition(targets, position);
  }
}

// `head ##0 tail`: the end point of a match of `head` and the start point of a
// match of `tail` are the same tick, so what holds there is both Booleans at
// once (LRM 16.7). A fused position is entered wherever the head position it
// came from was entered, which is what keeps the sharing the rest of the
// construction depends on.
auto Builder::Fuse(const SequenceFragment& head, const SequenceFragment& tail)
    -> SequenceFragment {
  SequenceFragment fused;
  for (const std::uint32_t ending : head.last) {
    for (const std::uint32_t starting : tail.first) {
      std::vector<hir::ExprId> conjuncts = positions_[ending].conjuncts;
      const std::vector<hir::ExprId>& tail_conjuncts =
          positions_[starting].conjuncts;
      conjuncts.insert(
          conjuncts.end(), tail_conjuncts.begin(), tail_conjuncts.end());

      const std::uint32_t merged = NewPosition(std::move(conjuncts));
      follow_[merged] = follow_[starting];
      if (Holds(tail.last, starting)) {
        AddPosition(fused.last, merged);
      }
      if (Holds(head.first, ending)) {
        AddPosition(fused.first, merged);
      }
      for (std::uint32_t source = 0; source < merged; ++source) {
        if (Holds(follow_[source], ending)) {
          AddPosition(follow_[source], merged);
        }
      }
    }
  }
  return fused;
}

// LRM 16.7: `head ##[m:n] tail`. The windows share one copy of the tail and one
// chain of the ticks a delay measures across, so a window states how far along
// that chain the tail may be entered rather than spelling out a sequence per
// width.
auto Builder::BuildDelay(const hir::SequenceDelay& delay) -> SequenceFragment {
  const SequenceFragment head = BuildSequence(delay.head);
  const SequenceFragment tail = BuildSequence(delay.tail);

  SequenceFragment result{.first = head.first, .last = tail.last};

  if (delay.delay.max >= 2) {
    Positions chain;
    chain.reserve(delay.delay.max - 1);
    for (std::uint32_t step = 1; step < delay.delay.max; ++step) {
      chain.push_back(NewPosition({}));
    }
    Connect(head.last, {chain.front()});
    for (std::size_t step = 0; step + 1 < chain.size(); ++step) {
      Connect({chain[step]}, {chain[step + 1]});
    }
    for (std::uint32_t width = std::max(delay.delay.min, 2U);
         width <= delay.delay.max; ++width) {
      Connect({chain[width - 2]}, tail.first);
    }
  }
  if (delay.delay.min <= 1 && delay.delay.max >= 1) {
    Connect(head.last, tail.first);
  }

  // Fusing last, so an end point that a wider window also leaves through keeps
  // the follow set that says so.
  if (delay.delay.min == 0) {
    const SequenceFragment fused = Fuse(head, tail);
    AddPositions(result.first, fused.first);
    AddPositions(result.last, fused.last);
    for (const std::uint32_t ending : head.last) {
      if (follow_[ending].empty()) {
        RemovePosition(result.first, ending);
        Unreach(ending);
      }
    }
  }
  return result;
}

// LRM 16.9.2: `body[*m:n]`, one copy per repetition because each is its own
// occurrence of the Boolean expressions, end point to start point. A count at
// or past the minimum is a match, which is what makes the window a set of end
// points over one chain.
auto Builder::BuildRepetition(const hir::SequenceRepetition& repetition)
    -> SequenceFragment {
  if (repetition.count.min == 0) {
    throw InternalError(
        "BuildRepetition: a repetition admitting an empty match is refused "
        "where it is read, so none reaches the automaton");
  }
  SequenceFragment result = BuildSequence(repetition.body);
  Positions ending = repetition.count.min <= 1 ? result.last : Positions{};
  SequenceFragment previous = result;
  for (std::uint32_t count = 2; count <= repetition.count.max; ++count) {
    const SequenceFragment copy = BuildSequence(repetition.body);
    Connect(previous.last, copy.first);
    if (count >= repetition.count.min) {
      AddPositions(ending, copy.last);
    }
    previous = copy;
  }
  result.last = std::move(ending);
  return result;
}

auto Builder::BuildSequence(hir::SequenceExprId id) -> SequenceFragment {
  const hir::SequenceExpr& expr = body_->sequence_exprs.Get(id);
  return std::visit(
      Overloaded{
          [&](const hir::SequenceBoolean& boolean) -> SequenceFragment {
            const std::uint32_t position = NewPosition({boolean.condition});
            return SequenceFragment{.first = {position}, .last = {position}};
          },
          [&](const hir::SequenceDelay& delay) -> SequenceFragment {
            return BuildDelay(delay);
          },
          [&](const hir::SequenceRepetition& repetition) -> SequenceFragment {
            return BuildRepetition(repetition);
          }},
      expr.data);
}

// A part is numbered when something first names it and built once the part
// naming it is done, so the parts are built outermost first and so are the
// seeds each records: the only way one seed reaches another is outward-in.
auto Builder::NewPart(hir::PropertyExprId property, bool negated)
    -> std::uint32_t {
  const auto index = static_cast<std::uint32_t>(parts_.size());
  parts_.emplace_back();
  unbuilt_.push_back(
      UnbuiltPart{.property = property, .negated = negated, .index = index});
  return index;
}

auto Builder::NewImplication() -> std::uint32_t {
  return implications_++;
}

void Builder::BuildPart(const UnbuiltPart& unbuilt) {
  std::vector<AnswerTerm> terms;
  const PropertyFragment answered = BuildAnswer(unbuilt.property, terms);
  if (unbuilt.negated) {
    terms.emplace_back(NegatedTerm{.operand = answered.term});
  }
  parts_[unbuilt.index] = AutomatonPart{
      .start = Compose(answered.start), .terms = std::move(terms)};
}

// `s |-> p`, `s |=> p`, and their duals `s #-# p`, `s #=# p`: each match of the
// antecedent starts an instance of the consequent, and the property is the
// least answer of those instances -- or, for the dual, of the instances of the
// consequent negated, negated again (LRM 16.12.7, 16.12.9).
auto Builder::BuildSeeded(
    hir::SequenceExprId antecedent, hir::PropertyExprId consequent,
    hir::ConsequentStart start, bool negated, std::vector<AnswerTerm>& terms)
    -> PropertyFragment {
  const auto before = static_cast<std::uint32_t>(positions_.size());
  const SequenceFragment fragment = BuildSequence(antecedent);
  Positions matches = fragment.last;
  switch (start) {
    case hir::ConsequentStart::kSameTick:
      break;
    // `s |=> p` is `(s ##1 1) |-> p` (Annex F.3.4.3.3): the tick after a match
    // is the antecedent's own, so a run ending before it leaves no match.
    case hir::ConsequentStart::kNextTick: {
      const std::uint32_t after = NewPosition({});
      Connect(fragment.last, {after});
      matches = {after};
      break;
    }
  }
  const Positions live = PositionsFrom(before, positions_.size());
  const std::uint32_t implication = NewImplication();
  seeds_.push_back(
      ImplicationSeed{
          .matches = Compose(matches),
          .implication = implication,
          .starts = MatchSeed{.part = NewPart(consequent, negated)}});
  terms.emplace_back(
      ImplicationTerm{.antecedent = Compose(live), .implication = implication});
  auto term = static_cast<std::uint32_t>(terms.size() - 1);
  if (negated) {
    terms.emplace_back(NegatedTerm{.operand = term});
    term = static_cast<std::uint32_t>(terms.size() - 1);
  }
  return PropertyFragment{.start = fragment.first, .term = term};
}

// An if-else or a case property reads what it selects by once, where it
// starts, and is then the arm that selected: a position matching
// unconditionally is where the selection is read, and the arm it starts is
// the one instance of its implication (LRM 16.12.6, 16.12.16; Annex F.3.4.3.4
// writes `if (b) p` as `b |-> p`).
auto Builder::BuildSelection(
    std::variant<MatchSeed, ConditionSeed, CaseSeed> starts,
    std::vector<AnswerTerm>& terms) -> PropertyFragment {
  const std::uint32_t selector = NewPosition({});
  const std::uint32_t implication = NewImplication();
  seeds_.push_back(
      ImplicationSeed{
          .matches = Compose({selector}),
          .implication = implication,
          .starts = std::move(starts)});
  terms.emplace_back(
      ImplicationTerm{
          .antecedent = Compose({selector}), .implication = implication});
  return PropertyFragment{
      .start = {selector},
      .term = static_cast<std::uint32_t>(terms.size() - 1)};
}

auto Builder::BuildAnswer(
    hir::PropertyExprId id, std::vector<AnswerTerm>& terms)
    -> PropertyFragment {
  const auto add = [&](AnswerTerm term) {
    terms.push_back(std::move(term));
    return static_cast<std::uint32_t>(terms.size() - 1);
  };
  const hir::PropertyExpr& expr = body_->property_exprs.Get(id);
  return std::visit(
      Overloaded{
          [&](const hir::PropertySequence& sequence) -> PropertyFragment {
            const auto before = static_cast<std::uint32_t>(positions_.size());
            const SequenceFragment fragment = BuildSequence(sequence.sequence);
            const Positions live = PositionsFrom(before, positions_.size());
            // A match is kept once made, so the sequence goes on answering
            // that it matched after every path through it has ended.
            const std::uint32_t kept = NewPosition({});
            follow_[kept] = {kept};
            Connect(fragment.last, {kept});
            return PropertyFragment{
                .start = fragment.first,
                .term =
                    add(SequenceTerm{
                        .live = Compose(live),
                        .kept = Compose({kept}),
                        .strength = sequence.strength})};
          },
          [&](const hir::PropertyImplication& implication) -> PropertyFragment {
            return BuildSeeded(
                implication.antecedent, implication.consequent,
                implication.start, false, terms);
          },
          [&](const hir::PropertyFollowedBy& followed) -> PropertyFragment {
            return BuildSeeded(
                followed.antecedent, followed.consequent, followed.start, true,
                terms);
          },
          [&](const hir::PropertyNot& negation) -> PropertyFragment {
            const PropertyFragment operand =
                BuildAnswer(negation.operand, terms);
            return PropertyFragment{
                .start = operand.start,
                .term = add(NegatedTerm{.operand = operand.term})};
          },
          [&](const hir::PropertyBinary& binary) -> PropertyFragment {
            const PropertyFragment lhs = BuildAnswer(binary.lhs, terms);
            const PropertyFragment rhs = BuildAnswer(binary.rhs, terms);
            Positions start = lhs.start;
            AddPositions(start, rhs.start);
            // LRM 16.12.8 defines `implies` and `iff` by what they answer
            // where each operand holds or fails, which Annex F.3.4.3.2 writes
            // as `not p1 or p2` and the conjunction of both directions.
            const auto implies = [&](std::uint32_t from, std::uint32_t to) {
              return add(
                  GreatestTerm{
                      .lhs = add(NegatedTerm{.operand = from}), .rhs = to});
            };
            std::uint32_t term = 0;
            switch (binary.connective) {
              case hir::PropertyConnective::kAnd:
                term = add(LeastTerm{.lhs = lhs.term, .rhs = rhs.term});
                break;
              case hir::PropertyConnective::kOr:
                term = add(GreatestTerm{.lhs = lhs.term, .rhs = rhs.term});
                break;
              case hir::PropertyConnective::kImplies:
                term = implies(lhs.term, rhs.term);
                break;
              case hir::PropertyConnective::kIff: {
                const std::uint32_t forward = implies(lhs.term, rhs.term);
                const std::uint32_t backward = implies(rhs.term, lhs.term);
                term = add(LeastTerm{.lhs = forward, .rhs = backward});
                break;
              }
            }
            return PropertyFragment{.start = std::move(start), .term = term};
          },
          [&](const hir::PropertyConditional& conditional) -> PropertyFragment {
            const std::uint32_t then_part =
                NewPart(conditional.then_property, false);
            std::optional<std::uint32_t> else_part;
            if (conditional.else_property.has_value()) {
              else_part = NewPart(*conditional.else_property, false);
            }
            return BuildSelection(
                ConditionSeed{
                    .condition = conditional.condition,
                    .then_part = then_part,
                    .else_part = else_part},
                terms);
          },
          [&](const hir::PropertyCase& selection) -> PropertyFragment {
            std::vector<CaseSeedItem> items;
            items.reserve(selection.items.size());
            for (const hir::PropertyCaseItem& item : selection.items) {
              items.push_back(
                  CaseSeedItem{
                      .labels = item.labels,
                      .part = NewPart(item.property, false)});
            }
            std::optional<std::uint32_t> default_part;
            if (selection.default_property.has_value()) {
              default_part = NewPart(*selection.default_property, false);
            }
            return BuildSelection(
                CaseSeed{
                    .subject = selection.subject,
                    .items = std::move(items),
                    .default_part = default_part},
                terms);
          }},
      expr.data);
}

auto Builder::Build(const hir::PropertySpec& spec) -> AssertionAutomaton {
  NewPart(spec.body, false);
  while (!unbuilt_.empty()) {
    const UnbuiltPart next = unbuilt_.front();
    unbuilt_.pop_front();
    BuildPart(next);
  }

  AssertionAutomaton automaton;
  automaton.words = static_cast<std::uint32_t>((positions_.size() + 63) / 64);
  const auto widen = [&](PositionSet& set) { set.resize(automaton.words, 0); };

  automaton.positions = std::move(positions_);
  automaton.follow.reserve(follow_.size());
  for (const Positions& targets : follow_) {
    PositionSet follow = Compose(targets);
    widen(follow);
    automaton.follow.push_back(std::move(follow));
  }
  for (AutomatonPart& part : parts_) {
    widen(part.start);
    for (AnswerTerm& term : part.terms) {
      std::visit(
          Overloaded{
              [&](SequenceTerm& sequence) {
                widen(sequence.live);
                widen(sequence.kept);
              },
              [&](ImplicationTerm& implication) {
                widen(implication.antecedent);
              },
              [](NegatedTerm&) {}, [](LeastTerm&) {}, [](GreatestTerm&) {}},
          term);
    }
  }
  for (ImplicationSeed& seed : seeds_) {
    widen(seed.matches);
  }
  automaton.parts = std::move(parts_);
  automaton.seeds = std::move(seeds_);
  return automaton;
}

}  // namespace

auto BuildAssertionAutomaton(
    const hir::ProceduralBody& body, const hir::PropertySpec& spec)
    -> AssertionAutomaton {
  return Builder{body}.Build(spec);
}

auto PositionSetOf(std::uint32_t position, std::uint32_t words) -> PositionSet {
  PositionSet set(words, 0);
  SetPosition(set, position);
  return set;
}

}  // namespace lyra::lowering::hir_to_mir
