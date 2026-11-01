#pragma once

#include <span>
#include <unordered_map>

#include "lyra/lowering/ast_to_hir/environment.hpp"
#include "lyra/lowering/ast_to_hir/outside_reach.hpp"
#include "lyra/lowering/ast_to_hir/parameter_value_source.hpp"

namespace slang::ast {
class InstanceSymbol;
class ParameterSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What follows of each elaborated instance from the body it is read through
// and from where it stands: what its body reaches outside itself, which of its
// parameters it is handed and where each takes its value from.
//
// Each answer is worked out from the source once and kept, which is why the
// lookups are const and the tables are not. What a body reaches outside itself
// is one answer for every instance read through that body, so it is kept under
// the instance the body was elaborated for; which parameters an instance is
// handed and what its ports are bound to are the instance's own, so those are
// kept under the instance asked.
class ElaboratedInstances {
 public:
  ElaboratedInstances() = default;
  ElaboratedInstances(const ElaboratedInstances&) = delete;
  auto operator=(const ElaboratedInstances&) -> ElaboratedInstances& = delete;
  ElaboratedInstances(ElaboratedInstances&&) = delete;
  auto operator=(ElaboratedInstances&&) -> ElaboratedInstances& = delete;
  ~ElaboratedInstances() = default;

  // The parameters `inst` is handed when it is built, in the order its body
  // declares them -- which is the order a construction supplies their values
  // in, for the parent writing the construction and the unit reading it alike.
  [[nodiscard]] auto SuppliedParametersOf(
      const slang::ast::InstanceSymbol& inst) const
      -> std::span<const slang::ast::ParameterSymbol* const>;

  // Where the value of `param`, declared anywhere in `inst`'s body, comes from
  // for the objects built from the compiled scope declaring it.
  [[nodiscard]] auto ValueSourceOf(
      const slang::ast::InstanceSymbol& inst,
      const slang::ast::ParameterSymbol& param) const -> ParameterValueSource;

  // What the body `inst` is read through reaches outside itself.
  [[nodiscard]] auto OutsideReachOf(
      const slang::ast::InstanceSymbol& inst) const -> const OutsideReach&;

  // What that reach comes to for `inst`.
  [[nodiscard]] auto EnvironmentOf(const slang::ast::InstanceSymbol& inst) const
      -> const Environment&;

  // Where each value parameter of `inst`'s body takes its value from.
  [[nodiscard]] auto ParametersOf(const slang::ast::InstanceSymbol& inst) const
      -> const ParametersOfInstance&;

 private:
  mutable std::unordered_map<
      const slang::ast::InstanceSymbol*, ParametersOfInstance>
      parameters_;
  mutable OutsideReaches reaches_;
  mutable std::unordered_map<const slang::ast::InstanceSymbol*, Environment>
      environments_;
};

}  // namespace lyra::lowering::ast_to_hir
