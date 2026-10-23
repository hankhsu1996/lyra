#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/runtime/constraint_circuit.hpp"

namespace CaDiCaL {
class Solver;
}

namespace lyra::runtime {

// Whether one condition of a constraint circuit can hold, asked again and
// again under different values of some of its inputs, of one solver instance
// that keeps what it learned between questions (CaDiCaL; Biere et al., SAT
// Competition 2024). Only the answer is offered: which assignment the solver
// found depends on what it learned before, so nothing drawn may read it.
class ConstraintSat {
 public:
  ConstraintSat(const ConstraintCircuit& circuit, CircuitLiteral condition);
  ConstraintSat(const ConstraintSat&) = delete;
  auto operator=(const ConstraintSat&) -> ConstraintSat& = delete;
  ConstraintSat(ConstraintSat&&) noexcept;
  auto operator=(ConstraintSat&&) noexcept -> ConstraintSat&;
  ~ConstraintSat();

  // Whether some assignment makes the condition true with each input of
  // `fixed` holding the value beside it.
  [[nodiscard]] auto Satisfiable(
      std::span<const std::pair<std::uint32_t, bool>> fixed) -> bool;

 private:
  std::unique_ptr<CaDiCaL::Solver> solver_;
  // The solver's variable for each input of the circuit, or 0 where the
  // condition does not read it.
  std::vector<int> variable_of_input_;
  // The last assignment found, by input: it makes the condition true, so a
  // question whose fixed values it agrees with is answered yes without asking
  // the solver. It decides only whether a question is asked, never its answer.
  std::optional<std::vector<bool>> witness_;
};

}  // namespace lyra::runtime
