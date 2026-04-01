#pragma once

#include <cstddef>
#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

struct BuiltinTypes {
  TypeId bit_type;
  TypeId logic_type;
  TypeId offset_type;
  TypeId string_type;
  TypeId void_type;
};

auto InternBuiltinTypes(TypeArena& arena) -> BuiltinTypes;

using PlaceMap = std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>;

// Map SymbolId -> mir::FunctionId (pre-allocated for recursion support)
using SymbolToMirFunctionMap =
    std::unordered_map<SymbolId, mir::FunctionId, SymbolIdHash>;

// Frozen design-global declarations produced by CollectDesignDeclarations().
// Contains only placement-facing / design-global data. Specialization-local
// body declarations (BodyLocalDecls) are collected separately per
// specialization group by CollectBodyLocalDecls().
//
// Immutable after construction; passed as const& to all lowering functions.
//
// Invariants (guaranteed on exit from CollectDesignDeclarations):
//   - design_places is complete for all design variables (pkg + module)
//   - functions is complete for all package functions
//   - All mir::FunctionIds in the map have frozen signatures in the arena
//     (written by ReserveFunction at pre-allocation time)
//   - slots.size() == num_design_slots
//   - No later phase may mutate any of the above
// Map module instance symbol -> index into InstanceTable (for %m support)
using InstanceIndexMap = std::unordered_map<SymbolId, uint32_t, SymbolIdHash>;

// Per-instance slot range within the design slot table.
// Recorded during CollectDesignDeclarations, parallel to module elements.
struct InstanceSlotRange {
  uint32_t slot_begin = 0;
  uint32_t slot_count = 0;
};

// Per-module body-local declaration map.
// Maps module-owned symbols to body-local PlaceIds (kModuleSlot, 0-based).
struct BodyLocalDecls {
  PlaceMap places;
  // Body-local slot descriptors (type + kind), indexed by kModuleSlot id.
  // Built directly as source of truth - not derived from design-global slots.
  std::vector<mir::SlotDesc> slots;
};

struct DesignDeclarations {
  // Design-global places: packages + design-level lowering.
  // For module-owned symbols, these are the design-global kDesignGlobal places
  // used by connection processes and design-level consumers.
  PlaceMap design_places;
  SymbolToMirFunctionMap functions;
  size_t num_design_slots = 0;
  // Number of package/global slots allocated before module-instance slots.
  // Set at the package/module boundary in CollectDesignDeclarations.
  // Used by placement to determine where module-instance storage begins.
  uint32_t num_package_slots = 0;
  // Slot descriptors: indexed by design slot ID, contains type and kind for
  // each slot. Ordering is ABI: packages first (in element order), then all
  // module instances (in BFS elaboration order).
  std::vector<mir::SlotDesc> slots;
  // Reverse lookup: module instance symbol -> instance table index.
  // Built from LoweringInput::instance_table during CollectDesignDeclarations.
  InstanceIndexMap instance_indices;
  // Per-module-instance slot ranges (parallel to module elements only).
  // Recorded in BFS elaboration order.
  std::vector<InstanceSlotRange> instance_slot_ranges;
  // Per-module-instance def IDs (parallel to instance_slot_ranges).
  std::vector<common::ModuleDefId> module_def_ids;
  // Per-module-instance param init entries (parallel to instance_slot_ranges).
  std::vector<std::vector<mir::ParamInitEntry>> instance_param_inits;

  // Compile-owned slot trace provenance (parallel to slots).
  std::vector<mir::SlotTraceProvenance> slot_trace_provenance;
  std::vector<char> slot_trace_string_pool;

  // Design-visible DPI imports from all packages and module bodies.
  DesignDpiImports dpi_imports;
  // Design-visible DPI exports from all packages and module bodies.
  // Exports with unsupported signatures are not registered; diagnostics
  // are collected in export_diagnostics instead.
  DesignDpiExports dpi_exports;

  // Diagnostics from export signature validation (unsupported types, etc.).
  // These are user-facing errors, not internal invariant violations.
  // The caller should surface them through the pipeline diagnostic sink.
  struct ExportDiagnostic {
    SourceSpan span;
    std::string message;
  };
  std::vector<ExportDiagnostic> export_diagnostics;
};

// Read-only view into declaration artifacts for lower-level helpers
// (LowerProcess, LowerFunctionBody). Ensures the frozen boundary is
// consistent throughout the lowering layer.
struct DeclView {
  const PlaceMap* body_places = nullptr;    // body-local (kModuleSlot)
  const PlaceMap* design_places = nullptr;  // design-global (kDesignGlobal)
  const SymbolToMirFunctionMap* functions = nullptr;
  const std::vector<mir::SlotDesc>* slots = nullptr;       // design-global
  const std::vector<mir::SlotDesc>* body_slots = nullptr;  // body-local
  // Design arena for cross-domain place resolution during body lowering.
  // Null for design-level lowering (init processes, connection processes).
  const mir::Arena* design_arena = nullptr;
  // Design-global function map for DesignFunctionRef resolution.
  // Null for design-level lowering.
  const SymbolToMirFunctionMap* design_functions = nullptr;
  // Design-level DPI import declarations visible to this lowering scope.
  // Null when no design-level declaration view is available.
  const DesignDpiImports* dpi_imports = nullptr;
};

// Result of AllocLocal - provides both the PlaceId and the local slot index.
// This makes the param_local_slots mapping explicit and avoids peeking at
// internal counters.
struct LocalAllocation {
  mir::PlaceId place;
  uint32_t local_slot = 0;
};

// Context for lowering within a process or function activation.
struct Context {
  mir::Arena* mir_arena;

  // Design arena for cross-domain place resolution. When set (body lowering),
  // LookupPlace creates body-local Places for design-global symbols instead
  // of returning design-arena PlaceIds. When null (design-level lowering),
  // design_places PlaceIds are returned directly.
  const mir::Arena* design_arena = nullptr;

  // Points to the body-local HIR arena during body lowering,
  // or design-global arena during design-level lowering.
  const hir::Arena* hir_arena;
  const TypeArena* type_arena;
  // Active constant domain: body-local constant arena during body
  // lowering, design-global constant arena during design-level lowering.
  const ConstantArena* active_constant_arena;
  const SymbolTable* symbol_table;

  // body_places: body-local (kModuleSlot) lookup, used for module body
  // lowering. design_places: design-global (kDesignGlobal) lookup, used for
  // design-level. Both must outlive this Context. local_places: process-scoped
  // storage, owned by this Context. Lookup order: local_places -> body_places
  // -> design_places.
  const PlaceMap* body_places = nullptr;
  const PlaceMap* design_places = nullptr;
  PlaceMap local_places;

  // Cache of body-local PlaceIds for design-global symbols.
  // Avoids creating duplicate body-local Places for the same design-global
  // symbol within one body. Only used during body lowering.
  mutable PlaceMap design_place_cache;

  int next_local_id = 0;
  int next_temp_id = 0;

  // Storage type tables - populated during allocation, not post-collection.
  // These grow as locals/temps are allocated.
  std::vector<TypeId> local_types;
  std::vector<TypeId> temp_types;  // deprecated: use temp_metadata instead

  // Authoritative temp metadata (indexed by temp_id).
  // Built during lowering, persisted to MIR Function/Process at finalization.
  std::vector<mir::TempMetadata> temp_metadata;

  BuiltinTypes builtin_types;

  // Function-specific: map symbols to MIR function IDs (for call lowering).
  // During body lowering, contains only body-local FunctionIds.
  const SymbolToMirFunctionMap* symbol_to_mir_function = nullptr;

  // Design-global function map for cross-domain callee resolution.
  // When set, ResolveCallTarget checks here first and returns
  // DesignFunctionRef for package functions. When null (design-level
  // lowering), all functions are resolved as arena-local FunctionIds.
  const SymbolToMirFunctionMap* design_functions = nullptr;

  // Design-level DPI import declarations visible to this lowering scope.
  // Null when no design-level declaration view is available.
  const DesignDpiImports* dpi_imports = nullptr;

  // Optional sink for dynamically generated functions (e.g., observer
  // programs). If set, LowerStrobeEffect will push program FunctionIds here.
  // Caller merges these into the parent element's functions list.
  std::vector<mir::FunctionId>* generated_functions = nullptr;

  // Single-exit form for functions: return_slot holds the return value,
  // exit_block is the common exit point where return_slot is loaded/returned.
  // Set by LowerFunctionBody for non-void functions; used by return lowering.
  std::optional<mir::PlaceId> return_slot;
  TypeId return_type = kInvalidTypeId;

  // Slot descriptors for write-protection checks.
  // design_slots: indexed by kDesignGlobal id.
  // body_slots: indexed by kModuleSlot id (body-local).
  const std::vector<mir::SlotDesc>* design_slots = nullptr;
  const std::vector<mir::SlotDesc>* body_slots = nullptr;

  // Stats: count of MaterializeOperandToPlace calls (for --stats output).
  uint64_t materialize_count = 0;

  auto AllocLocal(SymbolId sym, TypeId type) -> LocalAllocation;
  auto AllocTemp(TypeId type) -> mir::PlaceId;

  // Allocate a ValueTemp (SSA value, no storage).
  // Returns the temp_id to use with UseTemp operand.
  auto AllocValueTemp(TypeId type) -> int;

  // Throws InternalError if symbol not found (compiler bug, not user error).
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;

  // Resolve a PlaceId to its Place from the correct arena.
  // All PlaceIds in body MIR are body-arena-local, so this resolves
  // from mir_arena.
  [[nodiscard]] auto ResolvePlace(mir::PlaceId id) const -> const mir::Place& {
    return (*mir_arena)[id];
  }

  // Create a derived place in the body arena from a resolved base Place.
  // Convenience wrapper that resolves the base and calls Arena::DerivePlace.
  auto DerivePlace(mir::PlaceId base, mir::Projection proj) const
      -> mir::PlaceId {
    return mir_arena->DerivePlace(ResolvePlace(base), std::move(proj));
  }

  // Resolve a function symbol to its pre-allocated mir::FunctionId.
  // Throws InternalError if symbol not found (HIR guarantees all functions
  // are pre-allocated, so missing = compiler bug).
  [[nodiscard]] auto ResolveCallee(SymbolId sym) const -> mir::FunctionId;

  // Try to resolve a symbol as a DPI import. Returns nullopt if not a DPI
  // import. Used by call lowering to separate DPI from non-DPI call paths.
  [[nodiscard]] auto ResolveDpiImport(SymbolId sym) const
      -> std::optional<mir::DpiImportRef>;

  // Resolve a function symbol to a domain-aware callee reference.
  // Does not handle DPI imports (use ResolveDpiImport first).
  [[nodiscard]] auto ResolveCallTarget(SymbolId sym) const -> mir::Callee;

  // Resolve a callee to its function signature from the correct arena.
  // FunctionId reads from the body/design arena (mir_arena).
  // DesignFunctionRef reads from the design arena via symbol lookup.
  [[nodiscard]] auto ResolveCallSignature(const mir::Callee& callee) const
      -> const mir::FunctionSignature&;

  [[nodiscard]] auto GetBitType() const -> TypeId {
    return builtin_types.bit_type;
  }

  [[nodiscard]] auto GetUnitBitType(bool is_four_state) const -> TypeId {
    return is_four_state ? builtin_types.logic_type : builtin_types.bit_type;
  }

  [[nodiscard]] auto GetOffsetType() const -> TypeId {
    return builtin_types.offset_type;
  }

  [[nodiscard]] auto GetStringType() const -> TypeId {
    return builtin_types.string_type;
  }
};

}  // namespace lyra::lowering::hir_to_mir
