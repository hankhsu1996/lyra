#include "lyra/runtime/constraint_sampler.hpp"

#include <cstddef>
#include <cstdint>
#include <list>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/runtime/constraint_bdd.hpp"
#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/constraint_sat.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

namespace {

// How many values a variable tries before its bits are decided one by one,
// where a condition is drawn without a diagram. A value tried and kept fixes
// the whole variable at once, which reaches a value a constraint leaves few of
// far more often than deciding its bits does.
constexpr int kValuesTried = 5;

// How many bytes the diagrams kept and the conditions keying them hold
// together, past which the one drawn from longest ago is let go.
constexpr std::size_t kKeptBytes = std::size_t{64} << 20U;

struct DiagramKey {
  CanonicalCondition condition;
  std::size_t node_budget;

  auto operator==(const DiagramKey&) const -> bool = default;
};

struct DiagramKeyHash {
  auto operator()(const DiagramKey& key) const -> std::size_t {
    std::size_t seed = 0;
    base::HashCombine(seed, key.condition.drawn);
    base::HashCombine(seed, key.condition.others);
    base::HashCombine(seed, key.node_budget);
    for (const std::uint32_t word : key.condition.words) {
      base::HashCombine(seed, word);
    }
    return seed;
  }
};

// The diagrams built so far, by the condition they hold, kept for the next
// draw of the same condition; a condition past its budget is kept as none.
// Whether a diagram is kept never changes what a draw returns, since building
// it again gives the same diagram.
class DiagramStore {
 public:
  auto FindOrBuild(const DiagramKey& key)
      -> std::shared_ptr<const ConstraintBdd> {
    {
      const std::scoped_lock lock(mutex_);
      if (const auto it = entries_.find(key); it != entries_.end()) {
        recency_.splice(recency_.begin(), recency_, it->second.recency);
        return it->second.diagram;
      }
    }
    std::optional<ConstraintBdd> built =
        ConstraintBdd::Build(key.condition, key.node_budget);
    std::shared_ptr<const ConstraintBdd> diagram =
        built.has_value()
            ? std::make_shared<const ConstraintBdd>(*std::move(built))
            : nullptr;
    const std::scoped_lock lock(mutex_);
    if (const auto it = entries_.find(key); it != entries_.end()) {
      return it->second.diagram;
    }
    const std::size_t bytes =
        (key.condition.words.size() * sizeof(std::uint32_t)) +
        (diagram != nullptr ? diagram->HeldBytes() : 0);
    const auto at =
        entries_
            .emplace(
                key, Entry{.diagram = diagram, .bytes = bytes, .recency = {}})
            .first;
    recency_.push_front(&at->first);
    at->second.recency = recency_.begin();
    kept_ += bytes;
    while (kept_ > kKeptBytes && recency_.size() > 1) {
      const auto oldest = entries_.find(*recency_.back());
      kept_ -= oldest->second.bytes;
      recency_.pop_back();
      entries_.erase(oldest);
    }
    return diagram;
  }

 private:
  struct Entry {
    std::shared_ptr<const ConstraintBdd> diagram;
    std::size_t bytes;
    std::list<const DiagramKey*>::iterator recency;
  };

  std::mutex mutex_;
  std::unordered_map<DiagramKey, Entry, DiagramKeyHash> entries_;
  // The keys of `entries_`, the one drawn from last first.
  std::list<const DiagramKey*> recency_;
  std::size_t kept_ = 0;
};

auto Diagrams() -> DiagramStore& {
  static DiagramStore store;
  return store;
}

auto DrawBit(DrawRng& rng) -> bool {
  return (rng.NextValue() & 1U) != 0;
}

}  // namespace

ConstraintSampler::ConstraintSampler(
    const ConstraintCircuit& circuit, std::size_t node_budget)
    : circuit_(&circuit), node_budget_(node_budget) {
}

auto ConstraintSampler::Satisfiable(CircuitLiteral condition) const -> bool {
  if (condition.IsConstant()) {
    return condition == kTrueLiteral;
  }
  ConstraintSat sat(*circuit_, condition);
  return sat.Satisfiable({});
}

auto ConstraintSampler::Draw(
    CircuitLiteral condition, std::span<const DrawnVariable> drawn,
    DrawRng& rng) -> std::optional<std::vector<bool>> {
  if (condition == kFalseLiteral) {
    return std::nullopt;
  }
  // The order bits are decided in: the variables as listed, each from its
  // most significant bit.
  std::vector<std::uint32_t> order;
  for (const DrawnVariable& variable : drawn) {
    for (std::size_t i = variable.inputs.size(); i-- > 0;) {
      order.push_back(variable.inputs[i]);
    }
  }
  const std::shared_ptr<const ConstraintBdd> diagram = Diagrams().FindOrBuild(
      DiagramKey{
          .condition = CanonicalConditionOf(*circuit_, condition, order),
          .node_budget = node_budget_});
  if (diagram == nullptr) {
    drew_past_budget_ = true;
    return DrawBySearch(condition, drawn, rng);
  }
  if (diagram->Empty()) {
    return std::nullopt;
  }
  const std::vector<bool> decided = diagram->Draw(rng);
  std::vector<bool> out;
  out.reserve(order.size());
  std::size_t first = 0;
  for (const DrawnVariable& variable : drawn) {
    const std::size_t width = variable.inputs.size();
    for (std::size_t i = 0; i < width; ++i) {
      out.push_back(decided[first + width - 1 - i]);
    }
    first += width;
  }
  return out;
}

auto ConstraintSampler::DrawBySearch(
    CircuitLiteral condition, std::span<const DrawnVariable> drawn,
    DrawRng& rng) const -> std::optional<std::vector<bool>> {
  ConstraintSat sat(*circuit_, condition);
  if (!sat.Satisfiable({})) {
    return std::nullopt;
  }
  // Every value is decided by whether something completes it, never by which
  // completion the solver found, so what is drawn follows from the condition
  // and the generator alone.
  std::vector<std::pair<std::uint32_t, bool>> fixed;
  std::vector<bool> settled(drawn.size(), false);
  for (std::size_t v = 0; v < drawn.size(); ++v) {
    const DrawnVariable& variable = drawn[v];
    for (int tried = 0; tried < kValuesTried && !settled[v]; ++tried) {
      std::vector<bool> value(variable.inputs.size());
      if (!variable.members.empty()) {
        value = variable.members[DrawBelow(rng, variable.members.size())];
      } else {
        for (std::size_t i = value.size(); i-- > 0;) {
          value[i] = DrawBit(rng);
        }
      }
      std::vector<std::pair<std::uint32_t, bool>> attempt = fixed;
      for (std::size_t i = 0; i < value.size(); ++i) {
        attempt.emplace_back(variable.inputs[i], value[i]);
      }
      if (sat.Satisfiable(attempt)) {
        fixed = std::move(attempt);
        settled[v] = true;
      }
    }
  }
  for (std::size_t v = 0; v < drawn.size(); ++v) {
    if (settled[v]) {
      continue;
    }
    const DrawnVariable& variable = drawn[v];
    for (std::size_t i = variable.inputs.size(); i-- > 0;) {
      fixed.emplace_back(variable.inputs[i], DrawBit(rng));
      if (!sat.Satisfiable(fixed)) {
        fixed.back().second = !fixed.back().second;
      }
    }
  }
  std::unordered_map<std::uint32_t, bool> value_of(fixed.begin(), fixed.end());
  std::vector<bool> out;
  for (const DrawnVariable& variable : drawn) {
    for (const std::uint32_t input : variable.inputs) {
      out.push_back(value_of.at(input));
    }
  }
  return out;
}

}  // namespace lyra::runtime
