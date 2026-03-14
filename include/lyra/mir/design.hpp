#pragma once

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instance.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/placement.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t { kVariable, kNet, kParamConst };

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

struct SlotDesc {
  TypeId type;
  SlotKind kind = SlotKind::kVariable;
};

// Initialization entry for a promoted parameter slot.
// Slot is identified by absolute design slot index.
// Type is read from design.slots[slot_id].type (single source of truth).
struct ParamInitEntry {
  uint32_t slot_id;
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

  // Source of truth for all port connections.
  // Written ONLY by realization::AssembleBindings.
  std::vector<PortConnection> port_connections;

  // Wiring processes (separate from module processes).
  // These implement kDriveParentToChild and kDriveChildToParent semantics.
  // Written ONLY by realization::AssembleBindings.
  std::vector<ProcessId> connection_processes;

  // Instance table for %m support.
  // Index = instance_id, same order as all_instances during elaboration.
  InstanceTable instance_table;

  // Placement: source of truth for per-instance module placement in
  // DesignState. Covers module-instance storage only (not package/global
  // slots). Built from specialization bodies + instance records via running
  // base counter. Indexed by module-instance index (module-only elements in BFS
  // elaboration order), NOT parallel to Design::elements (which also
  // contains packages).
  PlacementMap placement;

  // DERIVED compatibility data: instance slot ranges derived from placement.
  // `placement` is the source of truth. New code should use placement helpers
  // (GetInstancePlacement, GetInstanceBaseSlot), not these tables.
  // These remain only for transitional compatibility with
  // layout/codegen/runtime.
  struct InstanceSlotRange {
    uint32_t slot_begin = 0;
    uint32_t slot_count = 0;
  };
  std::vector<InstanceSlotRange> instance_slot_ranges;
  // Per-module-instance def IDs (parallel to instance_slot_ranges).
  std::vector<common::ModuleDefId> module_def_ids;

  // Compile-owned slot trace provenance table (parallel to slots).
  // Each entry stores the local symbol name and scope ownership for one slot.
  // Final hierarchical names and trace kinds are derived at metadata lowering.
  std::vector<SlotTraceProvenance> slot_trace_provenance;
  std::vector<char> slot_trace_string_pool;
};

inline auto GetModuleBody(const Design& design, const Module& mod)
    -> const ModuleBody& {
  return design.module_bodies.at(mod.body_id.value);
}

}  // namespace lyra::mir
