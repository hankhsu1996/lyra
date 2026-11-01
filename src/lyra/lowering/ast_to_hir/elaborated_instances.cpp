#include "lyra/lowering/ast_to_hir/elaborated_instances.hpp"

#include <span>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>

#include "lyra/lowering/ast_to_hir/environment.hpp"
#include "lyra/lowering/ast_to_hir/outside_reach.hpp"
#include "lyra/lowering/ast_to_hir/parameter_value_source.hpp"

namespace lyra::lowering::ast_to_hir {

auto ElaboratedInstances::ParametersOf(const slang::ast::InstanceSymbol& inst)
    const -> const ParametersOfInstance& {
  if (const auto kept = parameters_.find(&inst); kept != parameters_.end()) {
    return kept->second;
  }
  return parameters_.emplace(&inst, ast_to_hir::ParametersOf(inst))
      .first->second;
}

auto ElaboratedInstances::SuppliedParametersOf(
    const slang::ast::InstanceSymbol& inst) const
    -> std::span<const slang::ast::ParameterSymbol* const> {
  return ParametersOf(inst).supplied;
}

auto ElaboratedInstances::ValueSourceOf(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) const -> ParameterValueSource {
  const ParametersOfInstance& known = ParametersOf(inst);
  const auto it = known.sources.find(&param);
  return it == known.sources.end() ? ParameterValueSource::kSettledAtElaboration
                                   : it->second;
}

auto ElaboratedInstances::OutsideReachOf(
    const slang::ast::InstanceSymbol& inst) const -> const OutsideReach& {
  return ast_to_hir::OutsideReachOf(inst, reaches_);
}

auto ElaboratedInstances::EnvironmentOf(
    const slang::ast::InstanceSymbol& inst) const -> const Environment& {
  if (const auto kept = environments_.find(&inst);
      kept != environments_.end()) {
    return kept->second;
  }
  return environments_.emplace(&inst, ast_to_hir::EnvironmentOf(inst, *this))
      .first->second;
}

}  // namespace lyra::lowering::ast_to_hir
