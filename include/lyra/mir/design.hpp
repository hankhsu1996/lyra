#pragma once

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/cover_site.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t { kVariable, kNet, kParamConst };

// Backend storage shape for a design-global slot.
// Determines whether the slot uses inline value storage or owned
// container storage with an out-of-line backing region.
enum class StorageShape : uint8_t {
  kInlineValue,     // Inline in the containing state object
  kOwnedContainer,  // Owned backing storage; inline handle + OOL region
};

// Scope ownership kind for trace provenance. Discriminates whether a slot
// belongs to a package or a module instance.
enum class SlotScopeKind : uint8_t { kPackage, kInstance };

// Compile-owned slot trace provenance. Compact per-slot record preserving
// the local symbol name and scope ownership from declaration time. Final
// hierarchical names and trace kinds are derived at metadata lowering time.
//
// Scope resolution:
//   kPackage  -- scope_ref is a string pool offset for the package name.
//   kInstance -- scope_ref is the instance table index (maps to instance_paths
//               at metadata lowering time).
//
// Trace kind is not stored here. It is derived from SlotDesc::kind
// (the existing slot table) during metadata lowering. This avoids
// duplicating kind information and keeps MIR free of trace-facing policy.
struct SlotTraceProvenance {
  uint32_t local_name_str_off = 0;
  SlotScopeKind scope_kind = SlotScopeKind::kInstance;
  uint32_t scope_ref = 0;
};

struct EventDesc {
  TypeId type;
};

struct SlotDesc {
  TypeId type;
  SlotKind kind = SlotKind::kVariable;
  // Backend storage-shape decision for this slot. Set during design-decl
  // lowering by ClassifySlotStorageShape(). Consumed by layout building
  // and codegen. Current rule: direct unpacked array roots in modules
  // with promoted parameters are classified as kOwnedContainer.
  // This is a conservative lowering approximation, not a semantic proof
  // of constructor-sized extent dependence.
  StorageShape storage_shape = StorageShape::kInlineValue;

  [[nodiscard]] auto IsOwnedContainer() const -> bool {
    return storage_shape == StorageShape::kOwnedContainer;
  }
};

// Initialization entry for a promoted parameter slot.
// Slot is identified by absolute design slot index.
// Type is read from design.slots[slot_id].type (single source of truth).
struct ParamInitEntry {
  uint32_t slot_id{};
  IntegralConstant value;
};

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
  std::vector<ModuleBody> module_bodies;
  size_t num_design_slots = 0;
  // Slot descriptors: indexed by design slot ID, contains type and kind for
  // each slot. Ordering is ABI: packages first (in element order), then all
  // module instances (in BFS elaboration order).
  // slots.size() == num_design_slots.
  std::vector<SlotDesc> slots;
  std::vector<ProcessId> init_processes;
  // Functions dynamically generated during lowering (e.g., observer programs
  // from init processes). These are not associated with any specific module.
  std::vector<FunctionId> generated_functions;

  // Global precision power for %t formatting (e.g., -12 for 1ps).
  // Set from compilation context, used by $timeformat defaults.
  int8_t global_precision_power = -9;

  // Connection data is in BoundConnection + expr_connections on the
  // lowering result, not here. These fields are empty.
  std::vector<PortConnection> port_connections;
  std::vector<ProcessId> connection_processes;

  // Compile-owned slot trace provenance table (parallel to slots).
  // Each entry stores the local symbol name and scope ownership for one slot.
  // Final hierarchical names and trace kinds are derived at metadata lowering.
  std::vector<SlotTraceProvenance> slot_trace_provenance;
  std::vector<char> slot_trace_string_pool;

  // Per-site metadata for immediate cover statements. One entry per
  // cover statement in the design, indexed by CoverSiteId. Populated
  // during HIR-to-MIR lowering. Runtime hit-count array is sized from
  // this table.
  std::vector<ImmediateCoverSiteInfo> immediate_cover_sites;

  // Per-site metadata for deferred immediate assertion statements.
  // One entry per deferred assertion site, indexed by DeferredAssertionSiteId.
  // Populated during HIR-to-MIR lowering. Runtime site metadata table is
  // built from this. Semantic only: no thunk IDs, payload layout, or
  // backend-specific data.
  std::vector<DeferredAssertionSiteInfo> deferred_assertion_sites;

  // Maximum body-local event count across all module bodies.
  // Emitted into the runtime ABI as a reserved/debug field. Per-body
  // event counts flow through BodyRealizationDesc; this global max
  // is for diagnostics only.
  size_t max_body_local_events = 0;
};

inline auto GetModuleBody(const Design& design, const Module& mod)
    -> const ModuleBody& {
  return design.module_bodies.at(mod.body_id.value);
}

}  // namespace lyra::mir
