#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

auto TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads,
    const UnitLoweringState& unit_state, const ScopeStack& stack)
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
    const auto* var = read.symbol->as_if<slang::ast::VariableSymbol>();
    if (var == nullptr) continue;
    const auto binding = unit_state.LookupStructuralVarBinding(*var);
    if (!binding.has_value()) continue;
    const auto hops = stack.HopsTo(binding->home_frame);
    if (!hops.has_value()) continue;
    out.push_back(
        hir::SensitivityEntry{
            .ref = hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
            .bit_range = read.bit_range});
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
