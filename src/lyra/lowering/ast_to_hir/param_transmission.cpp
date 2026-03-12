#include "lyra/lowering/ast_to_hir/param_transmission.hpp"

#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/types/Type.h>

namespace lyra::lowering::ast_to_hir {

auto ParamTransmissionTable::Lookup(const slang::ast::ParameterSymbol& p) const
    -> ParamDisposition {
  return transmitted_params_.contains(&p) ? ParamDisposition::kTransmitted
                                          : ParamDisposition::kAbsorbed;
}

auto DeriveParamTransmission(
    const common::SpecializationMap& spec_map,
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> ParamTransmissionTable {
  ParamTransmissionTable table;

  for (const auto& group : spec_map.groups) {
    // Single-instance groups have no within-group variance.
    if (group.instance_indices.size() < 2) continue;

    // Use the first instance's body as reference.
    const auto& ref_body = all_instances[group.instance_indices[0]]->body;

    // Collect parameter members from the reference body.
    std::vector<const slang::ast::ParameterSymbol*> params;
    for (const auto& member : ref_body.members()) {
      if (member.kind == slang::ast::SymbolKind::Parameter) {
        params.push_back(&member.as<slang::ast::ParameterSymbol>());
      }
    }

    // Compare each parameter across all instances in the group.
    // Only integral params can be transmitted (the runtime slot system
    // only supports IntegralConstant). Non-integral params (string, real)
    // are always absorbed regardless of variance.
    for (const auto* ref_param : params) {
      if (!ref_param->getType().isIntegral()) continue;

      const auto& ref_value = ref_param->getValue();
      bool has_variance = false;

      for (size_t i = 1; i < group.instance_indices.size(); ++i) {
        const auto& other_body = all_instances[group.instance_indices[i]]->body;
        const auto* other_sym = other_body.find(ref_param->name);
        if (other_sym == nullptr) continue;
        if (other_sym->kind != slang::ast::SymbolKind::Parameter) continue;
        const auto& other_param = other_sym->as<slang::ast::ParameterSymbol>();
        if (other_param.getValue() != ref_value) {
          has_variance = true;
          break;
        }
      }

      if (!has_variance) continue;

      // Mark as transmitted in ALL instances of this group.
      table.MarkTransmitted(ref_param);
      for (size_t i = 1; i < group.instance_indices.size(); ++i) {
        const auto& other_body = all_instances[group.instance_indices[i]]->body;
        const auto* other_sym = other_body.find(ref_param->name);
        if (other_sym == nullptr) continue;
        if (other_sym->kind != slang::ast::SymbolKind::Parameter) continue;
        table.MarkTransmitted(&other_sym->as<slang::ast::ParameterSymbol>());
      }
    }
  }

  return table;
}

}  // namespace lyra::lowering::ast_to_hir
