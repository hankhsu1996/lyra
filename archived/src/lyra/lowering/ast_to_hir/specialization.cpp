#include "lyra/lowering/ast_to_hir/specialization.hpp"

#include <map>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>

#include "absl/hash/hash.h"
#include "lyra/common/module_identity.hpp"
#include "lyra/lowering/ast_to_hir/repertoire_descriptor.hpp"

namespace lyra::lowering::ast_to_hir {
namespace {

// Compile-owned representation fingerprint from the definition's type store.
//
// Only the load-bearing subset of type entries is hashed -- those that
// independently change compiled representation (LLVM IR shape):
//
//   Included: IntegralDesc, PackedArrayDesc, PackedStructDesc,
//             PackedUnionDesc, EnumDesc
//
//   Excluded: unpacked containers (UnpackedArrayDesc, UnpackedStructDesc,
//             UnpackedUnionDesc, DynamicArrayDesc, QueueDesc,
//             AssociativeArrayDesc) and kind-only entries (void, real,
//             short_real, string).
//
// Unpacked/container entries only reference element TypeDescIds. They can
// only differ between instances when their element's packed type differs --
// but that packed type is itself a separate entry already hashed here. So
// container entries are redundant for specialization identity and hashing
// them would teach the wrong idea that all type-store entries define
// specialization.
//
// The type store is definition-scoped because BuildArtifactInventory walks
// all generate branches (slang's body.members() includes uninstantiated
// GenerateBlockSymbols). So the store contains types from ALL constructor
// alternatives, not just the instantiated branch.
auto HashCompileOwnedTypeStore(const CompileOwnedTypeStore& store) -> uint64_t {
  size_t h = 0;

  for (const auto& entry : store.entries) {
    switch (entry.kind) {
      case TypeDescKind::kIntegral: {
        const auto& p = std::get<IntegralDesc>(entry.payload);
        h = absl::HashOf(h, static_cast<uint8_t>(entry.kind));
        h = absl::HashOf(h, p.bit_width, p.is_signed, p.is_four_state);
        break;
      }
      case TypeDescKind::kPackedArray: {
        const auto& p = std::get<PackedArrayDesc>(entry.payload);
        h = absl::HashOf(h, static_cast<uint8_t>(entry.kind));
        h = absl::HashOf(h, p.element.value, p.left, p.right);
        break;
      }
      case TypeDescKind::kPackedStruct: {
        const auto& p = std::get<PackedStructDesc>(entry.payload);
        h = absl::HashOf(h, static_cast<uint8_t>(entry.kind));
        h = absl::HashOf(h, p.total_bit_width, p.is_signed, p.is_four_state);
        for (const auto& field : p.fields) {
          h = absl::HashOf(
              h, field.type.value, field.bit_offset, field.bit_width);
        }
        break;
      }
      case TypeDescKind::kPackedUnion: {
        const auto& p = std::get<PackedUnionDesc>(entry.payload);
        h = absl::HashOf(h, static_cast<uint8_t>(entry.kind));
        h = absl::HashOf(h, p.total_bit_width, p.is_signed, p.is_four_state);
        for (const auto& member : p.members) {
          h = absl::HashOf(
              h, member.type.value, member.bit_offset, member.bit_width);
        }
        break;
      }
      case TypeDescKind::kEnum: {
        const auto& p = std::get<EnumDesc>(entry.payload);
        h = absl::HashOf(h, static_cast<uint8_t>(entry.kind));
        h = absl::HashOf(h, p.base_type.value);
        for (const auto& member : p.members) {
          h = absl::HashOf(h, member.value);
        }
        break;
      }
      default:
        break;
    }
  }

  return h;
}

}  // namespace

auto ComputeStructuralFingerprint(const slang::ast::InstanceBodySymbol& body)
    -> common::StructuralFingerprint {
  const auto desc = BuildDefinitionRepertoireDesc(body);
  return common::StructuralFingerprint{
      HashCompileOwnedTypeStore(desc.type_store)};
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
