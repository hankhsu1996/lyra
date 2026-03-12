#include "lyra/lowering/ast_to_hir/specialization.hpp"

#include <algorithm>
#include <map>
#include <string>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "absl/hash/hash.h"
#include "lyra/common/module_identity.hpp"

namespace lyra::lowering::ast_to_hir {
namespace {

// Compile-owned declaration descriptor: name and resolved type of a single
// module-local variable or net.
//
// v1: uses slang's type.toString() as canonical type representation.
// Pragmatic starting point -- not a principled canonicalizer.
//
// Risk: type.toString() may encode runtime-owned differences (e.g., unpacked
// container sizing) that should not split specialization. If this causes
// oversplitting, the canonicalizer needs narrowing. Tracked as part of M2c.
struct CompileOwnedDeclDescriptor {
  std::string name;
  std::string type_repr;
};

// Compile-owned body descriptor for the v1 discriminator.
// Sorted by name for deterministic hashing.
//
// v1 scope: module-local variables and nets only. This captures
// declaration-side compile-owned shape (packed widths, signedness, 2-state
// vs 4-state as reflected in resolved types). It does not capture
// procedural/behavioral compile-owned shape (e.g., generate branch
// selections that affect code shape without introducing new declarations).
// Tracked as M2c.
struct CompileOwnedBodyDescriptor {
  std::vector<CompileOwnedDeclDescriptor> decls;
};

// Returns true if a body member is a compile-owned declaration.
//
// v1 includes all module-local variables and nets. If a parameter affects
// packed width, signedness, or any other type property, the resolved type
// of the affected variable/net will differ, producing a different fingerprint.
//
// Excludes parameters, localparams, child instances, generates, procedures.
// Their declaration-side effects are observed indirectly through the
// resolved types of the variables/nets they influence.
auto IsCompileOwnedDeclaration(const slang::ast::Symbol& member) -> bool {
  return member.kind == slang::ast::SymbolKind::Variable ||
         member.kind == slang::ast::SymbolKind::Net;
}

// Returns the canonical type representation for a compile-owned declaration.
//
// v1: reuses slang's type.toString(). See CompileOwnedDeclDescriptor comment
// for the runtime-owned type risk.
auto CanonicalCompileOwnedTypeRepr(const slang::ast::Type& type)
    -> std::string {
  return type.toString();
}

auto BuildCompileOwnedBodyDescriptor(const slang::ast::InstanceBodySymbol& body)
    -> CompileOwnedBodyDescriptor {
  CompileOwnedBodyDescriptor desc;

  for (const auto& member : body.members()) {
    if (!IsCompileOwnedDeclaration(member)) {
      continue;
    }
    const auto& val = member.as<slang::ast::ValueSymbol>();
    desc.decls.push_back(
        CompileOwnedDeclDescriptor{
            .name = std::string(val.name),
            .type_repr = CanonicalCompileOwnedTypeRepr(val.getType()),
        });
  }

  // Sort by name for deterministic hashing. Module-local declaration names
  // are unique within a scope (guaranteed by slang's elaboration model),
  // so name-only sorting is sufficient for a stable ordering.
  std::ranges::sort(
      desc.decls, [](const auto& a, const auto& b) { return a.name < b.name; });

  return desc;
}

// Hash a descriptor into a v1 declaration-shape fingerprint.
// Hashes only body-local declaration facts (name + type_repr pairs).
// def_id is not included -- it is paired externally by BuildSpecializationMap.
auto HashCompileOwnedBodyDescriptor(const CompileOwnedBodyDescriptor& desc)
    -> common::StructuralFingerprint {
  size_t h = 0;
  for (const auto& decl : desc.decls) {
    h = absl::HashOf(h, decl.name, decl.type_repr);
  }
  return common::StructuralFingerprint{h};
}

}  // namespace

auto ComputeStructuralFingerprint(const slang::ast::InstanceBodySymbol& body)
    -> common::StructuralFingerprint {
  auto desc = BuildCompileOwnedBodyDescriptor(body);
  return HashCompileOwnedBodyDescriptor(desc);
}

auto BuildSpecializationMap(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> common::SpecializationMap {
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
    auto fingerprint = ComputeStructuralFingerprint(all_instances[i]->body);
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
    SpecIdKey key{
        .def_id = spec_id.def_id.value,
        .fingerprint = spec_id.fingerprint.value};
    groups[key].push_back(static_cast<common::ModuleInstanceIndex>(i));
  }

  result.groups.reserve(groups.size());
  for (auto& [key, indices] : groups) {
    result.groups.push_back(
        common::SpecializationGroup{
            .spec_id =
                common::ModuleSpecId{
                    .def_id = common::ModuleDefId{key.def_id},
                    .fingerprint =
                        common::StructuralFingerprint{key.fingerprint}},
            .instance_indices = std::move(indices),
        });
  }

  return result;
}

}  // namespace lyra::lowering::ast_to_hir
