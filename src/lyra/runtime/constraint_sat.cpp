#include "lyra/runtime/constraint_sat.hpp"

#include <algorithm>
#include <cadical.hpp>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/constraint_circuit.hpp"

namespace lyra::runtime {

namespace {

// CaDiCaL's answers to `solve`.
constexpr int kSatisfiable = 10;
constexpr int kUnsatisfiable = 20;

}  // namespace

ConstraintSat::ConstraintSat(
    const ConstraintCircuit& circuit, CircuitLiteral condition)
    : solver_(std::make_unique<CaDiCaL::Solver>()),
      variable_of_input_(circuit.InputCount(), 0) {
  // One variable per gate and input the condition reads, each gate tied to
  // its inputs by three clauses (Tseitin's encoding), and a clause making the
  // condition true.
  std::vector<int> variable_of(circuit.NodeCount(), 0);
  const int constant = solver_->declare_one_more_variable();
  solver_->add(-constant);
  solver_->add(0);
  variable_of[0] = constant;
  const auto literal = [&](CircuitLiteral l) {
    const int variable = variable_of[l.Node()];
    return l.IsNegated() ? -variable : variable;
  };
  std::vector<bool> seen(circuit.NodeCount(), false);
  std::vector<std::uint32_t> cone;
  std::vector<std::uint32_t> stack{condition.Node()};
  while (!stack.empty()) {
    const std::uint32_t node = stack.back();
    stack.pop_back();
    if (node == 0 || seen[node]) {
      continue;
    }
    seen[node] = true;
    cone.push_back(node);
    if (!circuit.IsInput(node)) {
      const auto [left, right] = circuit.FaninOf(node);
      stack.push_back(left.Node());
      stack.push_back(right.Node());
    }
  }
  // A gate is made after its inputs, so the order made puts each after them.
  std::ranges::sort(cone);
  for (const std::uint32_t node : cone) {
    const int variable = solver_->declare_one_more_variable();
    variable_of[node] = variable;
    if (circuit.IsInput(node)) {
      variable_of_input_[circuit.InputIndex(node)] = variable;
      continue;
    }
    const auto [left, right] = circuit.FaninOf(node);
    for (const int clause :
         {-variable, literal(left), 0, -variable, literal(right), 0, variable,
          -literal(left), -literal(right), 0}) {
      solver_->add(clause);
    }
  }
  solver_->add(literal(condition));
  solver_->add(0);
}

ConstraintSat::ConstraintSat(ConstraintSat&&) noexcept = default;
auto ConstraintSat::operator=(ConstraintSat&&) noexcept
    -> ConstraintSat& = default;
ConstraintSat::~ConstraintSat() = default;

auto ConstraintSat::Satisfiable(
    std::span<const std::pair<std::uint32_t, bool>> fixed) -> bool {
  const bool agrees =
      witness_.has_value() && std::ranges::all_of(fixed, [&](const auto& pair) {
        return variable_of_input_[pair.first] == 0 ||
               (*witness_)[pair.first] == pair.second;
      });
  if (agrees) {
    return true;
  }
  for (const auto& [input, value] : fixed) {
    // An input the condition does not read takes any value, which no
    // assignment making the condition true rules out.
    if (const int variable = variable_of_input_[input]; variable != 0) {
      solver_->assume(value ? variable : -variable);
    }
  }
  const int answer = solver_->solve();
  if (answer == kUnsatisfiable) {
    return false;
  }
  if (answer != kSatisfiable) {
    throw InternalError(
        "ConstraintSat: the solver stopped without an answer -- please report "
        "this as a bug");
  }
  std::vector<bool> witness(variable_of_input_.size(), false);
  for (std::size_t input = 0; input < variable_of_input_.size(); ++input) {
    if (const int variable = variable_of_input_[input]; variable != 0) {
      witness[input] = solver_->val(variable) > 0;
    }
  }
  witness_ = std::move(witness);
  return true;
}

}  // namespace lyra::runtime
