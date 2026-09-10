#include "lyra/lowering/hir_to_mir/assertion_automaton.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
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
// stands for it. Every set an automaton hands out is spelled through here.
void SetPosition(PositionSet& set, std::uint32_t position) {
  set[position / 64] |= std::uint64_t{1} << (position % 64);
}

// The automaton as it is built: positions carry indices, which is what a set
// operation reads, and the machine words a transition reads them in are
// composed once at the end.
class Builder {
 public:
  explicit Builder(const hir::ProceduralBody& body) : body_(&body) {
  }

  [[nodiscard]] auto Build(const hir::PropertySpec& spec) -> AssertionAutomaton;

 private:
  // A sequence as its two boundaries: where a match of it may begin, and where
  // one may end. Everything between is already in the follow relation.
  struct Fragment {
    Positions first;
    Positions last;
  };

  [[nodiscard]] auto NewPosition(std::vector<hir::ExprId> conjuncts)
      -> std::uint32_t;
  [[nodiscard]] auto BuildSequence(hir::SequenceExprId id) -> Fragment;
  [[nodiscard]] auto BuildDelay(const hir::SequenceDelay& delay) -> Fragment;
  [[nodiscard]] auto BuildRepetition(const hir::SequenceRepetition& repetition)
      -> Fragment;
  [[nodiscard]] auto BuildProperty(hir::PropertyExprId id) -> Positions;
  void Connect(const Positions& from, const Positions& to);
  [[nodiscard]] auto Fuse(const Fragment& head, const Fragment& tail)
      -> Fragment;
  void Unreach(std::uint32_t position);

  const hir::ProceduralBody* body_;
  std::vector<AssertionPosition> positions_;
  std::vector<Positions> follow_;
  Positions satisfying_;
  Positions trigger_;
  // One implication each, as the boundaries a seed connects: composing them
  // into words needs the position count, which is not settled until the whole
  // property is built.
  std::vector<std::pair<Positions, Positions>> seed_boundaries_;
  std::vector<bool> seed_same_tick_;
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
auto Builder::Fuse(const Fragment& head, const Fragment& tail) -> Fragment {
  Fragment fused;
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
auto Builder::BuildDelay(const hir::SequenceDelay& delay) -> Fragment {
  const Fragment head = BuildSequence(delay.head);
  const Fragment tail = BuildSequence(delay.tail);

  Fragment result{.first = head.first, .last = tail.last};

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
    const Fragment fused = Fuse(head, tail);
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
    -> Fragment {
  if (repetition.count.min == 0) {
    throw InternalError(
        "BuildRepetition: a repetition admitting an empty match is refused "
        "where it is read, so none reaches the automaton");
  }
  Fragment result = BuildSequence(repetition.body);
  Positions ending = repetition.count.min <= 1 ? result.last : Positions{};
  Fragment previous = result;
  for (std::uint32_t count = 2; count <= repetition.count.max; ++count) {
    const Fragment copy = BuildSequence(repetition.body);
    Connect(previous.last, copy.first);
    if (count >= repetition.count.min) {
      AddPositions(ending, copy.last);
    }
    previous = copy;
  }
  result.last = std::move(ending);
  return result;
}

auto Builder::BuildSequence(hir::SequenceExprId id) -> Fragment {
  const hir::SequenceExpr& expr = body_->sequence_exprs.Get(id);
  return std::visit(
      Overloaded{
          [&](const hir::SequenceBoolean& boolean) -> Fragment {
            const std::uint32_t position = NewPosition({boolean.condition});
            return Fragment{.first = {position}, .last = {position}};
          },
          [&](const hir::SequenceDelay& delay) -> Fragment {
            return BuildDelay(delay);
          },
          [&](const hir::SequenceRepetition& repetition) -> Fragment {
            return BuildRepetition(repetition);
          }},
      expr.data);
}

auto Builder::BuildProperty(hir::PropertyExprId id) -> Positions {
  const hir::PropertyExpr& expr = body_->property_exprs.Get(id);
  return std::visit(
      Overloaded{
          [&](const hir::PropertySequence& sequence) -> Positions {
            const Fragment fragment = BuildSequence(sequence.sequence);
            AddPositions(satisfying_, fragment.last);
            return fragment.first;
          },
          [&](const hir::PropertyImplication& implication) -> Positions {
            const auto before = static_cast<std::uint32_t>(positions_.size());
            const Fragment antecedent = BuildSequence(implication.antecedent);
            for (auto position = before;
                 position < static_cast<std::uint32_t>(positions_.size());
                 ++position) {
              AddPosition(trigger_, position);
            }
            // Recorded before the consequent is built, so a seed always comes
            // before the seeds nested inside it. A reader taking one forward
            // pass over them then has the whole same-tick cascade, because the
            // only way one seed reaches another is outward-in.
            const std::size_t slot = seed_boundaries_.size();
            seed_boundaries_.emplace_back();
            seed_same_tick_.push_back(
                implication.start == hir::ImplicationStart::kSameTick);
            Positions consequent = BuildProperty(implication.consequent);
            seed_boundaries_[slot] = {antecedent.last, std::move(consequent)};
            return antecedent.first;
          }},
      expr.data);
}

auto Builder::Build(const hir::PropertySpec& spec) -> AssertionAutomaton {
  const Positions start = BuildProperty(spec.body);

  AssertionAutomaton automaton;
  automaton.words = static_cast<std::uint32_t>((positions_.size() + 63) / 64);
  const auto compose = [&](const Positions& set) {
    PositionSet words(automaton.words, 0);
    for (const std::uint32_t position : set) {
      SetPosition(words, position);
    }
    return words;
  };

  automaton.positions = std::move(positions_);
  automaton.follow.reserve(follow_.size());
  for (const Positions& targets : follow_) {
    automaton.follow.push_back(compose(targets));
  }
  automaton.start = compose(start);
  automaton.satisfying = compose(satisfying_);
  automaton.trigger = compose(trigger_);
  automaton.seeds.reserve(seed_boundaries_.size());
  for (std::size_t index = 0; index < seed_boundaries_.size(); ++index) {
    automaton.seeds.push_back(
        ImplicationSeed{
            .antecedent_last = compose(seed_boundaries_[index].first),
            .consequent_start = compose(seed_boundaries_[index].second),
            .same_tick = seed_same_tick_[index]});
  }
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
