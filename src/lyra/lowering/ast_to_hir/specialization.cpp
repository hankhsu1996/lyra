#include "lyra/lowering/ast_to_hir/specialization.hpp"

#include <cstdint>
#include <map>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>

#include "absl/hash/hash.h"
#include "lyra/common/module_identity.hpp"
#include "lyra/lowering/ast_to_hir/param_role.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Hashable representation of a single structural parameter value.
// Position-based (parameter identity comes from definition order +
// ModuleDefId).
struct StructuralParamEntry {
  uint32_t bit_width;
  bool is_signed;
  std::vector<uint64_t> words;

  template <typename H>
  friend auto AbslHashValue(H h, const StructuralParamEntry& e) -> H {
    h = H::combine(std::move(h), e.bit_width, e.is_signed);
    h = H::combine_contiguous(std::move(h), e.words.data(), e.words.size());
    return h;
  }
};

}  // namespace

auto ComputeStructuralFingerprint(
    const slang::ast::InstanceBodySymbol& body,
    const ParamRoleTable& param_roles) -> common::StructuralFingerprint {
  std::vector<StructuralParamEntry> entries;

  for (const auto& member : body.members()) {
    if (member.kind != slang::ast::SymbolKind::Parameter) {
      continue;
    }
    const auto& param = member.as<slang::ast::ParameterSymbol>();
    if (param_roles.Lookup(param) != ParamRole::kShape) {
      continue;
    }

    // Only hash integer-valued parameters.
    // Non-integer params (real, string) are not structural.
    if (!param.getValue().isInteger()) {
      continue;
    }

    const slang::SVInt& sv_int = param.getValue().integer();
    uint32_t num_words = sv_int.getNumWords();
    const uint64_t* raw = sv_int.getRawPtr();

    entries.push_back(
        StructuralParamEntry{
            .bit_width = sv_int.getBitWidth(),
            .is_signed = sv_int.isSigned(),
            .words = std::vector<uint64_t>(raw, raw + num_words),
        });
  }

  return common::StructuralFingerprint{absl::HashOf(entries)};
}

auto BuildSpecializationMap(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances,
    const ParamRoleTable& param_roles) -> common::SpecializationMap {
  common::SpecializationMap result;
  result.spec_id_by_instance.reserve(all_instances.size());

  // Assign ModuleDefId per DefinitionSymbol*
  uint32_t next_def_id = 0;
  std::unordered_map<const slang::ast::DefinitionSymbol*, uint32_t> def_ids;

  for (size_t i = 0; i < all_instances.size(); ++i) {
    const auto& def = all_instances[i]->body.getDefinition();
    auto [it, inserted] = def_ids.try_emplace(&def, next_def_id);
    if (inserted) ++next_def_id;

    common::ModuleDefId def_id{it->second};
    auto fingerprint =
        ComputeStructuralFingerprint(all_instances[i]->body, param_roles);
    result.spec_id_by_instance.push_back({def_id, fingerprint});
  }

  // Group instances by ModuleSpecId.
  // std::map ensures deterministic ordering by (def_id, fingerprint).
  struct SpecIdKey {
    uint32_t def_id;
    uint64_t fingerprint;
    auto operator<=>(const SpecIdKey&) const = default;
  };
  std::map<SpecIdKey, std::vector<common::ModuleInstanceIndex>> groups;

  for (size_t i = 0; i < all_instances.size(); ++i) {
    const auto& spec_id = result.spec_id_by_instance[i];
    SpecIdKey key{spec_id.def_id.value, spec_id.fingerprint.value};
    groups[key].push_back(static_cast<common::ModuleInstanceIndex>(i));
  }

  result.groups.reserve(groups.size());
  for (auto& [key, indices] : groups) {
    result.groups.push_back(
        common::SpecializationGroup{
            .spec_id =
                common::ModuleSpecId{
                    common::ModuleDefId{key.def_id},
                    common::StructuralFingerprint{key.fingerprint}},
            .instance_indices = std::move(indices),
        });
  }

  return result;
}

}  // namespace lyra::lowering::ast_to_hir
