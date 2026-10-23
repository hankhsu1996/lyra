#include "lyra/runtime/constraint_bdd.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cudd.h>
#include <map>
#include <mutex>
#include <optional>
#include <span>
#include <tuple>
#include <util.h>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/natural.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

auto CanonicalConditionOf(
    const ConstraintCircuit& circuit, CircuitLiteral condition,
    std::span<const std::uint32_t> drawn) -> CanonicalCondition {
  CanonicalCondition out;
  out.drawn = static_cast<std::uint32_t>(drawn.size());
  std::vector<std::int64_t> drawn_at(circuit.InputCount(), -1);
  for (std::size_t i = 0; i < drawn.size(); ++i) {
    drawn_at[drawn[i]] = static_cast<std::int64_t>(i);
  }
  // The walk's finishing order: every node of the condition's cone after the
  // nodes it reads.
  std::vector<std::uint32_t> finished;
  std::vector<bool> seen(circuit.NodeCount(), false);
  std::vector<std::pair<std::uint32_t, bool>> stack{{condition.Node(), false}};
  while (!stack.empty()) {
    const auto [node, expanded] = stack.back();
    stack.pop_back();
    if (expanded) {
      finished.push_back(node);
      continue;
    }
    if (node == 0 || seen[node]) {
      continue;
    }
    seen[node] = true;
    stack.emplace_back(node, true);
    if (!circuit.IsInput(node)) {
      const auto [left, right] = circuit.FaninOf(node);
      stack.emplace_back(right.Node(), false);
      stack.emplace_back(left.Node(), false);
    }
  }
  std::vector<std::uint32_t> other_at(circuit.NodeCount(), 0);
  std::vector<std::uint32_t> gate_at(circuit.NodeCount(), 0);
  std::uint32_t gates = 0;
  for (const std::uint32_t node : finished) {
    if (!circuit.IsInput(node)) {
      gate_at[node] = gates++;
    } else if (drawn_at[circuit.InputIndex(node)] < 0) {
      other_at[node] = out.others++;
    }
  }
  const auto number = [&](std::uint32_t node) -> std::uint32_t {
    if (node == 0) {
      return 0;
    }
    if (circuit.IsInput(node)) {
      const std::int64_t at = drawn_at[circuit.InputIndex(node)];
      return at >= 0 ? 1 + static_cast<std::uint32_t>(at)
                     : 1 + out.drawn + other_at[node];
    }
    return 1 + out.drawn + out.others + gate_at[node];
  };
  const auto literal = [&](CircuitLiteral l) {
    return (number(l.Node()) << 1U) | (l.IsNegated() ? 1U : 0U);
  };
  for (const std::uint32_t node : finished) {
    if (!circuit.IsInput(node)) {
      const auto [left, right] = circuit.FaninOf(node);
      out.words.push_back(literal(left));
      out.words.push_back(literal(right));
    }
  }
  out.words.push_back(literal(condition));
  return out;
}

namespace {

// How many entries the package's table of computed results starts with; it
// grows as the build finds it useful.
constexpr unsigned int kFirstCacheSlots = 1024;
// What the package may hold for each node the budget allows: the node, its
// share of the unique table, and the computed results kept beside it.
constexpr std::size_t kBytesPerBudgetedNode = 256;

// A manager of its own, so how the diagram grows follows from the condition
// alone: no node another diagram left, and no reordering of the levels.
class Manager {
 public:
  Manager(std::uint32_t variables, std::size_t node_budget)
      : dd_(Init(variables, node_budget)) {
    if (dd_ == nullptr) {
      throw SimulationError(
          "'randomize' ran out of memory solving its constraints");
    }
    Cudd_AutodynDisable(dd_);
    Cudd_RegisterOutOfMemoryCallback(dd_, Cudd_OutOfMemSilent);
  }
  Manager(const Manager&) = delete;
  auto operator=(const Manager&) -> Manager& = delete;
  Manager(Manager&&) = delete;
  auto operator=(Manager&&) -> Manager& = delete;
  ~Manager() {
    Cudd_Quit(dd_);
  }
  [[nodiscard]] auto Get() const -> DdManager* {
    return dd_;
  }

 private:
  // The package ends the process where an allocation fails unless told
  // otherwise, so it is told to answer nothing instead, before its first
  // allocation; the failure then stops the simulation as any other would.
  //
  // Its tables start small and grow with the diagram, toward a memory target
  // set by the budget rather than by what the machine allows, so a condition
  // of a few nodes costs a few nodes' work to build.
  static auto Init(std::uint32_t variables, std::size_t node_budget)
      -> DdManager* {
    static std::once_flag silenced;
    std::call_once(silenced, [] { MMoutOfMemory = Cudd_OutOfMemSilent; });
    return Cudd_Init(
        variables, 0, CUDD_UNIQUE_SLOTS, kFirstCacheSlots,
        node_budget * kBytesPerBudgetedNode);
  }

  DdManager* dd_;
};

// Stops the simulation unless an operation that answered nothing stopped at
// the limit it was given, which is how a condition too large for the budget
// ends.
void CheckStoppedAtLimit(DdManager* dd) {
  const Cudd_ErrorType error = Cudd_ReadErrorCode(dd);
  if (error == CUDD_MEMORY_OUT) {
    throw SimulationError(
        "'randomize' ran out of memory solving its constraints");
  }
  if (error != CUDD_TOO_MANY_NODES && error != CUDD_NO_ERROR) {
    throw InternalError(
        "ConstraintBdd: the decision-diagram package failed -- please report "
        "this as a bug");
  }
}

}  // namespace

auto ConstraintBdd::Build(
    const CanonicalCondition& condition, std::size_t node_budget)
    -> std::optional<ConstraintBdd> {
  const std::uint32_t variables = condition.drawn + condition.others;
  const Manager manager(variables, node_budget);
  DdManager* const dd = manager.Get();
  const std::size_t gates = (condition.words.size() - 1) / 2;
  // Each numbered node's function, referenced while some later gate or the
  // condition still reads it.
  std::vector<DdNode*> of(1 + variables + gates, nullptr);
  of[0] = Cudd_ReadLogicZero(dd);
  for (std::uint32_t v = 0; v < variables; ++v) {
    of[1 + v] = Cudd_bddIthVar(dd, static_cast<int>(v));
  }
  std::vector<std::uint32_t> readers(of.size(), 0);
  for (const std::uint32_t word : condition.words) {
    ++readers[word >> 1U];
  }
  const auto read = [&](std::uint32_t word) {
    DdNode* const f = of[word >> 1U];
    return (word & 1U) != 0 ? Cudd_Not(f) : f;
  };
  const auto release = [&](std::uint32_t word) {
    const std::uint32_t node = word >> 1U;
    if (node > variables && --readers[node] == 0) {
      Cudd_RecursiveDeref(dd, of[node]);
      of[node] = nullptr;
    }
  };
  const auto release_all = [&] {
    for (std::size_t node = 1 + variables; node < of.size(); ++node) {
      if (of[node] != nullptr) {
        Cudd_RecursiveDeref(dd, of[node]);
        of[node] = nullptr;
      }
    }
  };
  // Live nodes are counted from here, so the variables' own nodes are not
  // charged to the condition.
  const long baseline = Cudd_ReadNodeCount(dd);
  const auto room = [&]() -> std::optional<unsigned int> {
    const long live = Cudd_ReadNodeCount(dd) - baseline;
    if (live >= static_cast<long>(node_budget)) {
      return std::nullopt;
    }
    return static_cast<unsigned int>(static_cast<long>(node_budget) - live);
  };
  for (std::size_t g = 0; g < gates; ++g) {
    const std::uint32_t left = condition.words[2 * g];
    const std::uint32_t right = condition.words[(2 * g) + 1];
    const std::optional<unsigned int> limit = room();
    DdNode* const f =
        limit.has_value()
            ? Cudd_bddAndLimit(dd, read(left), read(right), *limit)
            : nullptr;
    if (f == nullptr) {
      CheckStoppedAtLimit(dd);
      release_all();
      return std::nullopt;
    }
    Cudd_Ref(f);
    of[1 + variables + g] = f;
    release(left);
    release(right);
  }
  const std::uint32_t root = condition.words.back();
  DdNode* const whole = read(root);
  Cudd_Ref(whole);
  release(root);

  // The other inputs summed out: true wherever some value of them makes the
  // condition true.
  std::vector<int> others(condition.others);
  for (std::uint32_t i = 0; i < condition.others; ++i) {
    others[i] = static_cast<int>(condition.drawn + i);
  }
  DdNode* const cube =
      Cudd_IndicesToCube(dd, others.data(), static_cast<int>(others.size()));
  if (cube == nullptr) {
    CheckStoppedAtLimit(dd);
    Cudd_RecursiveDeref(dd, whole);
    return std::nullopt;
  }
  Cudd_Ref(cube);
  const std::optional<unsigned int> limit = room();
  DdNode* const projected =
      limit.has_value() ? Cudd_bddExistAbstractLimit(dd, whole, cube, *limit)
                        : nullptr;
  Cudd_RecursiveDeref(dd, cube);
  Cudd_RecursiveDeref(dd, whole);
  if (projected == nullptr) {
    CheckStoppedAtLimit(dd);
    return std::nullopt;
  }
  Cudd_Ref(projected);

  // The diagram copied out with each negated edge carried down to the
  // terminals, so it reads without the package, and counted bottom up.
  ConstraintBdd out;
  out.levels_ = condition.drawn;
  out.nodes_ = {
      Node{.level = out.levels_, .low = 0, .high = 0},
      Node{.level = out.levels_, .low = 1, .high = 1}};
  out.counts_ = {Natural{}, Natural{1}};
  std::map<std::pair<DdNode*, bool>, std::uint32_t> copied;
  // Each entry is a node and whether its edge is negated, and whether its
  // children are copied already.
  std::vector<std::tuple<DdNode*, bool, bool>> stack{
      {Cudd_Regular(projected), Cudd_IsComplement(projected), false}};
  const auto copy_of = [&](DdNode* node, bool negated) -> std::uint32_t {
    if (Cudd_IsConstant(node)) {
      // The constant node is true, and false read negated.
      return negated ? 0 : 1;
    }
    return copied.at({node, negated});
  };
  while (!stack.empty()) {
    const auto [node, negated, children_done] = stack.back();
    stack.pop_back();
    if (Cudd_IsConstant(node) || copied.contains({node, negated})) {
      continue;
    }
    DdNode* const then_child = Cudd_T(node);
    DdNode* const else_child = Cudd_E(node);
    const bool then_negated = negated != (Cudd_IsComplement(then_child) != 0);
    const bool else_negated = negated != (Cudd_IsComplement(else_child) != 0);
    if (!children_done) {
      stack.emplace_back(node, negated, true);
      stack.emplace_back(Cudd_Regular(then_child), then_negated, false);
      stack.emplace_back(Cudd_Regular(else_child), else_negated, false);
      continue;
    }
    const std::uint32_t high = copy_of(Cudd_Regular(then_child), then_negated);
    const std::uint32_t low = copy_of(Cudd_Regular(else_child), else_negated);
    const std::uint32_t level = Cudd_NodeReadIndex(node);
    const auto weight = [&](std::uint32_t child) {
      return out.counts_[child].ShiftedLeft(
          out.nodes_[child].level - level - 1);
    };
    out.counts_.push_back(weight(low) + weight(high));
    out.nodes_.push_back(Node{.level = level, .low = low, .high = high});
    copied.emplace(
        std::pair{node, negated},
        static_cast<std::uint32_t>(out.nodes_.size() - 1));
  }
  out.root_ = copy_of(Cudd_Regular(projected), Cudd_IsComplement(projected));
  Cudd_RecursiveDeref(dd, projected);
  return out;
}

auto ConstraintBdd::HeldBytes() const -> std::size_t {
  std::size_t bytes = nodes_.size() * sizeof(Node);
  for (const Natural& count : counts_) {
    bytes += count.HeldBytes();
  }
  return bytes;
}

auto ConstraintBdd::Empty() const -> bool {
  return root_ == 0;
}

auto ConstraintBdd::Draw(DrawRng& rng) const -> std::vector<bool> {
  if (Empty()) {
    throw InternalError(
        "ConstraintBdd: a draw from a condition nothing makes true -- please "
        "report this as a bug");
  }
  std::vector<bool> bits(levels_, false);
  // A number below how many values the diagram holds names one of them: the
  // levels a branch skips take its low bits, and each node takes its low
  // branch where the number falls among the values below it, which keeps
  // every value one number.
  const std::uint32_t above = nodes_[root_].level;
  Natural rank = Natural::DrawBelow(rng, counts_[root_].ShiftedLeft(above));
  const auto take_free = [&](std::uint32_t from, std::uint32_t count) {
    const std::vector<bool> free = rank.TakeLowBits(count);
    for (std::uint32_t i = 0; i < count; ++i) {
      bits[from + i] = free[i];
    }
  };
  take_free(0, above);
  std::uint32_t node = root_;
  while (node > 1) {
    const Node& at = nodes_[node];
    const std::uint32_t low_gap = nodes_[at.low].level - at.level - 1;
    const Natural low_weight = counts_[at.low].ShiftedLeft(low_gap);
    std::uint32_t next = at.low;
    if (rank < low_weight) {
      bits[at.level] = false;
    } else {
      rank = rank - low_weight;
      bits[at.level] = true;
      next = at.high;
    }
    take_free(at.level + 1, nodes_[next].level - at.level - 1);
    node = next;
  }
  return bits;
}

}  // namespace lyra::runtime
