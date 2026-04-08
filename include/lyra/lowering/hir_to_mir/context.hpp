#pragma once

#include <cstddef>
#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/selection_step.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/cover_site.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

struct BuiltinTypes {
  TypeId bit_type;
  TypeId logic_type;
  TypeId offset_type;
  TypeId string_type;
  TypeId void_type;
  TypeId i8_type;   // 8-bit unsigned 2-state (for decision observation)
  TypeId i16_type;  // 16-bit unsigned 2-state (for decision observation)
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
//   - design_places is complete for all package variables (package-only)
//   - functions is complete for all package functions
//   - All mir::FunctionIds in the map have frozen signatures in the arena
//     (written by ReserveFunction at pre-allocation time)
//   - slots.size() == num_design_slots
//   - No later phase may mutate any of the above
// Map module instance symbol -> index into InstanceTable (for %m support)
using InstanceIndexMap = std::unordered_map<SymbolId, uint32_t, SymbolIdHash>;

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

// Kind of a provisional path step in a hierarchical reference.
enum class ProvisionalPathStepKind : uint8_t {
  // Traversal into a child module instance. Changes object_index during
  // topology walk.
  kChildInstance,
  // Traversal into a generate scope (genblk, for-generate, if-generate).
  // Stays within the same object. Does not change object_index.
  kGenerateScope,
};

// Provisional path step for non-local target resolution during lowering.
struct ProvisionalPathStep {
  ProvisionalPathStepKind kind = ProvisionalPathStepKind::kChildInstance;
  SymbolId sym;
  common::RepertoireCoord coord;
  auto operator==(const ProvisionalPathStep&) const -> bool = default;
};

// Provisional non-local target built during HIR->MIR lowering.
// Contains the upward navigation + descendant path + final target slot.
// Stored alongside ExternalAccessRecipe as provenance for Phase 3
// canonicalization. Phase 2 uses SymbolId-keyed caching; Phase 3 will
// refine to full recipe key.
struct ProvisionalNonLocalTarget {
  mir::NonLocalAnchor anchor = mir::NonLocalAnchor::kSelf;
  uint32_t upward_count = 0;
  std::vector<ProvisionalPathStep> path;
  common::LocalSlotId target_slot;
  // Target variable symbol for post-pass resolution. Required for
  // FinalizeExternalRefTargetSlots to look up the target's LocalSlotId.
  SymbolId target_sym;
  auto operator==(const ProvisionalNonLocalTarget&) const -> bool = default;
};

// Read-only view into declaration artifacts for lower-level helpers
// (LowerProcess, LowerFunctionBody). Ensures the frozen boundary is
// consistent throughout the lowering layer.
struct DeclView {
  const PlaceMap* body_places = nullptr;    // body-local (kModuleSlot)
  const PlaceMap* design_places = nullptr;  // package-global (kDesignGlobal)
  // Cross-instance places. NOT part of LookupPlace in body lowering.
  // V3d residual: used by design-level LookupPlace fallback only.
  const PlaceMap* cross_instance_places = nullptr;
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
  // Shared design-global registry for immediate cover site allocation.
  // Null for lowering scopes that cannot contain cover statements
  // (e.g., package init lowering).
  mir::ImmediateCoverSiteRegistry* cover_site_registry = nullptr;
  // Shared design-global registry for deferred assertion site allocation.
  // Null for lowering scopes that cannot contain deferred assertions.
  mir::DeferredAssertionSiteRegistry* deferred_assertion_site_registry =
      nullptr;
  // B2: External ref registry for body lowering. When non-null, hierarchical
  // refs produce ExternalRefId operands. Null for design-level lowering.
  std::vector<mir::ExternalAccessRecipe>* external_refs = nullptr;
  // B2: Provisional non-local targets (parallel to external_refs).
  // Populated during lowering, consumed during Phase 3 canonicalization.
  std::vector<ProvisionalNonLocalTarget>* provisional_targets = nullptr;
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
  const PlaceMap* design_places = nullptr;  // package-only
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

  // Cross-instance place map. V3d residual: used by design-level
  // LookupPlace fallback and connection compilation only. Not accessed
  // by body-lowering hierarchical read/write or sensitivity paths.
  const PlaceMap* cross_instance_places = nullptr;

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

  // Shared design-global registry for immediate cover site allocation.
  // Owned by the design lowering scope; all body/process contexts share
  // the same registry to produce dense design-global site IDs. Null when
  // cover sites are not being tracked (e.g., design-level init lowering).
  mir::ImmediateCoverSiteRegistry* cover_site_registry = nullptr;
  mir::DeferredAssertionSiteRegistry* deferred_assertion_site_registry =
      nullptr;

  auto AllocateCoverSite(SourceSpan span) const -> mir::CoverSiteId {
    if (cover_site_registry == nullptr) {
      throw common::InternalError(
          "Context::AllocateCoverSite", "cover site registry not initialized");
    }
    return cover_site_registry->Allocate(span);
  }

  auto AllocateDeferredAssertionSite(mir::DeferredAssertionSiteInfo info) const
      -> mir::DeferredAssertionSiteId {
    if (deferred_assertion_site_registry == nullptr) {
      throw common::InternalError(
          "Context::AllocateDeferredAssertionSite",
          "deferred assertion site registry not initialized");
    }
    return deferred_assertion_site_registry->Allocate(std::move(info));
  }

  // Stats: count of MaterializeOperandToPlace calls (for --stats output).
  uint64_t materialize_count = 0;

  // B2: External ref registry for body lowering.
  // When non-null, LowerHierarchicalRefToExternalRef emits ExternalRefId
  // operands instead of resolving to kDesignGlobal PlaceIds.
  // Null for design-level lowering (init processes, connection processes).
  std::vector<mir::ExternalAccessRecipe>* external_refs = nullptr;
  // B2: Provisional non-local targets (parallel to external_refs).
  std::vector<ProvisionalNonLocalTarget>* provisional_targets = nullptr;
  // Cache by full provisional recipe identity (access_kind + target +
  // upward_count + path + target_sym). Two different paths to the same
  // symbol get different ExternalRefIds.
  struct ExternalRefPathIdentityStep {
    hir::HierPathStepKind kind;
    SymbolId sym;
    common::RepertoireCoord selection;
    auto operator==(const ExternalRefPathIdentityStep&) const -> bool = default;
  };
  struct ExternalRefKey {
    mir::ExternalAccessKind access_kind;
    SymbolId target_sym;
    uint32_t upward_count;
    std::vector<ExternalRefPathIdentityStep> path_identity;
    auto operator==(const ExternalRefKey&) const -> bool = default;
  };
  struct ExternalRefKeyHash {
    auto operator()(const ExternalRefKey& key) const noexcept -> size_t {
      size_t h = std::hash<uint8_t>{}(static_cast<uint8_t>(key.access_kind));
      h ^= std::hash<uint32_t>{}(key.target_sym.value) << 1;
      h ^= std::hash<uint32_t>{}(key.upward_count) << 2;
      for (const auto& step : key.path_identity) {
        h ^= std::hash<uint8_t>{}(static_cast<uint8_t>(step.kind)) << 3;
        h ^= std::hash<uint32_t>{}(step.sym.value) << 4;
        // Hash full selection content, not just size.
        for (const auto& sel : step.selection) {
          h ^= std::hash<uint32_t>{}(static_cast<uint32_t>(sel.kind)) << 5;
          h ^= std::hash<uint32_t>{}(sel.construct_index) << 6;
          h ^= std::hash<uint32_t>{}(sel.alt_index) << 7;
        }
      }
      return h;
    }
  };
  std::unordered_map<ExternalRefKey, mir::ExternalRefId, ExternalRefKeyHash>
      external_ref_cache;

  auto AllocLocal(SymbolId sym, TypeId type) -> LocalAllocation;
  auto AllocTemp(TypeId type) -> mir::PlaceId;

  // Allocate a ValueTemp (SSA value, no storage).
  // Returns the temp_id to use with UseTemp operand.
  auto AllocValueTemp(TypeId type) -> int;

  // Throws InternalError if symbol not found (compiler bug, not user error).
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;

  // B2: Result of lowering a hierarchical ref to an external ref.
  struct ExternalRefResult {
    mir::ExternalRefId ref_id;
    TypeId type;
  };

  // B2: Lower a hierarchical reference to an ExternalRefId.
  // Allocates a new ExternalAccessRecipe (and provisional target) if not
  // cached for this symbol. Requires external_refs to be non-null (body
  // lowering only).
  // access: the kind of access being performed (read, write, or read-write).
  auto LowerHierarchicalRefToExternalRef(
      const hir::HierarchicalRefExpressionData& data, TypeId type,
      mir::ExternalAccessKind access) -> ExternalRefResult;

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

  [[nodiscard]] auto GetI8Type() const -> TypeId {
    return builtin_types.i8_type;
  }

  [[nodiscard]] auto GetI16Type() const -> TypeId {
    return builtin_types.i16_type;
  }
};

}  // namespace lyra::lowering::hir_to_mir
