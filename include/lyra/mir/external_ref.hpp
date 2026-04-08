#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <vector>

#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/object_index.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/selection_step.hpp"
#include "lyra/common/type.hpp"

namespace lyra::mir {

// Body-local handle for a non-local access requirement.
// Indexes into CompiledModuleBody::external_refs.
struct ExternalRefId {
  uint32_t value = 0;
  auto operator==(const ExternalRefId&) const -> bool = default;
};

// Declares the required access kind for an external reference.
// A kRead ref lowers to loads through the bound handle at runtime.
// A kWrite ref lowers to stores. A kReadWrite supports both.
// External refs are first-class places for both reads and writes;
// one kExternalRef operand/statement pathway handles both directions.
enum class ExternalAccessKind : uint8_t {
  kRead,
  kWrite,
  kReadWrite,
};

// Root anchor for the non-local path. Only two anchors:
// kSelf = path starts from the current body's scope.
// kPackage = target is a package-global (no hierarchy traversal).
enum class NonLocalAnchor : uint8_t {
  kSelf,
  kPackage,
};

// Durable child instance identity within a parent body.
//
// This is the canonical identity for construction-time binding. It must
// distinguish all siblings (including multiple non-generate children at
// the same scope) and all generated realizations.
//
// The identity is (coord, child_ordinal):
//   coord: RepertoireCoord from the parent's repertoire descriptor.
//     Identifies the generate-scope context (empty for non-generate).
//   child_ordinal: deterministic encounter order of child instances
//     within the same (coord, kChildInstance) bucket. Distinguishes
//     siblings that share the same generate scope.
//
// This pair is sufficient and canonical. ChildBindingSiteId is a
// separate compile-time indexing aid that does not appear here.
struct DurableChildId {
  common::RepertoireCoord coord;
  uint32_t child_ordinal = 0;

  auto operator==(const DurableChildId&) const -> bool = default;
};

struct DurableChildIdHash {
  auto operator()(const DurableChildId& id) const noexcept -> size_t {
    size_t h = std::hash<uint32_t>{}(id.child_ordinal);
    for (const auto& step : id.coord) {
      h ^= std::hash<uint32_t>{}(static_cast<uint32_t>(step.kind)) * 31;
      h ^= std::hash<uint32_t>{}(step.construct_index) * 37;
      h ^= std::hash<uint32_t>{}(step.alt_index) * 41;
    }
    return h;
  }
};

// One downward step through a child instance in the hierarchy.
// Carries the durable child identity directly -- self-sufficient for
// construction-time binding without out-of-band lookup.
struct DescendantPathStep {
  DurableChildId child;

  auto operator==(const DescendantPathStep&) const -> bool = default;
};

// Durable non-local target identity for construction-time binding.
//
// Root: anchor determines the starting scope (kSelf or kPackage).
// Upward: upward_count is the number of parent-hops from self before
//   descending. 0 means the path starts from the current body itself.
//   1 means one parent hop, etc. Mirrors slang's upwardCount.
// Path: descendant steps from the starting scope to the target body.
// Leaf: target_slot within the final body.
struct NonLocalTargetRecipe {
  NonLocalAnchor anchor = NonLocalAnchor::kSelf;
  uint32_t upward_count = 0;
  std::vector<DescendantPathStep> path;
  common::LocalSlotId target_slot;
};

// Body-local declaration of a non-local access requirement.
// Compile time knows the type, access kind, and durable target path.
// The actual storage location is resolved at construction time.
struct ExternalAccessRecipe {
  ExternalRefId ref_id;
  TypeId type;
  ExternalAccessKind access_kind = ExternalAccessKind::kRead;
  common::OriginId origin = common::OriginId::Invalid();
  NonLocalTargetRecipe target;
};

// Durable binding fact for a resolved external reference.
// Produced by HIR-to-MIR topology resolution, consumed by LLVM backend.
// Contains only semantic identity (object + slot + type), not layout data.
// Backend computes design-global address from ConstructionInput at codegen
// time.
//
// Two kinds of bindings:
//   Instance-owned: target_object + target_local_slot identify the storage.
//     Backend resolves via obj.design_state_base_slot + local_slot.
//   Package/global: global_slot directly identifies a design-global slot.
//     Currently unused (package globals use design_places, not ExternalRefId).
//     Reserved for future NonLocalAnchor::kPackage support.
struct ResolvedExternalRefBinding {
  common::ObjectIndex target_object;
  common::LocalSlotId target_local_slot;
  TypeId type;
  // Set for package/global targets (no target_object traversal needed).
  // nullopt for instance-owned targets.
  std::optional<uint32_t> global_slot;

  [[nodiscard]] auto IsPackageOrGlobal() const -> bool {
    return global_slot.has_value();
  }

  [[nodiscard]] auto GlobalSlotId() const -> uint32_t {
    return *global_slot;
  }
};

}  // namespace lyra::mir
