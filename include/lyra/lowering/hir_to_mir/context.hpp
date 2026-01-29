#pragma once

#include <cstddef>
#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

struct BuiltinTypes {
  // 2-state bit[1] for MIR control predicates (branch conditions, case
  // comparisons, validity flags). MIR control predicates are always 2-state
  // by construction - any SV 4-state-to-boolean conversion happens at the
  // point of lowering an expression into a control predicate.
  TypeId bit_type;
  TypeId offset_type;
  TypeId string_type;
  TypeId void_type;  // For void return types
};

auto InternBuiltinTypes(TypeArena& arena) -> BuiltinTypes;

struct SymbolIdHash {
  auto operator()(SymbolId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

using PlaceMap = std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>;

// Map SymbolId -> mir::FunctionId (pre-allocated for recursion support)
using SymbolToMirFunctionMap =
    std::unordered_map<SymbolId, mir::FunctionId, SymbolIdHash>;

// Frozen design-level declarations produced by CollectDeclarations().
// Immutable after construction; passed as const& to all lowering functions.
//
// Invariants (guaranteed on exit from CollectDeclarations):
//   - design_places is complete for all design variables (pkg + module)
//   - functions is complete for all package functions
//   - All mir::FunctionIds in the map have frozen signatures in the arena
//     (written by ReserveFunction at pre-allocation time)
//   - slot_table.size() == num_design_slots
//   - No later phase may mutate any of the above
// Map module instance symbol → index into InstanceTable (for %m support)
using InstanceIndexMap = std::unordered_map<SymbolId, uint32_t, SymbolIdHash>;

struct DesignDeclarations {
  PlaceMap design_places;
  SymbolToMirFunctionMap functions;
  size_t num_design_slots = 0;
  // Slot table: indexed by design slot ID, contains TypeId for each slot.
  // Ordering is ABI: packages first (in element order), then all module
  // instances (in BFS elaboration order).
  std::vector<TypeId> slot_table;
  // Reverse lookup: module instance symbol → instance table index.
  // Built from LoweringInput::instance_table during CollectDeclarations.
  InstanceIndexMap instance_indices;
};

// Read-only view into declaration artifacts for lower-level helpers
// (LowerProcess, LowerFunctionBody). Ensures the frozen boundary is
// consistent throughout the lowering layer.
struct DeclView {
  const PlaceMap* places;
  const SymbolToMirFunctionMap* functions;
};

// Result of AllocLocal - provides both the PlaceId and the local slot index.
// This makes the param_local_slots mapping explicit and avoids peeking at
// internal counters.
struct LocalAllocation {
  mir::PlaceId place;
  uint32_t local_slot;
};

// Context for lowering within a process or function activation.
struct Context {
  mir::Arena* mir_arena;

  const hir::Arena* hir_arena;
  const TypeArena* type_arena;
  const ConstantArena* constant_arena;
  const SymbolTable* symbol_table;

  // module_places: module-scoped storage, must outlive this Context.
  // local_places: process-scoped storage, owned by this Context.
  const PlaceMap* module_places;
  PlaceMap local_places;

  int next_local_id = 0;
  int next_temp_id = 0;

  // Storage type tables - populated during allocation, not post-collection.
  // These grow as locals/temps are allocated.
  std::vector<TypeId> local_types;
  std::vector<TypeId> temp_types;

  BuiltinTypes builtin_types;

  // Function-specific: map symbols to MIR function IDs (for call lowering)
  const SymbolToMirFunctionMap* symbol_to_mir_function = nullptr;

  // Optional sink for dynamically generated functions (e.g., strobe thunks).
  // If set, LowerStrobeEffect will push thunk FunctionIds here.
  // Caller merges these into the parent element's functions list.
  std::vector<mir::FunctionId>* generated_functions = nullptr;

  // Single-exit form for functions: return_slot holds the return value,
  // exit_block is the common exit point where return_slot is loaded/returned.
  // Set by LowerFunctionBody for non-void functions; used by return lowering.
  std::optional<mir::PlaceId> return_slot;
  TypeId return_type = kInvalidTypeId;

  auto AllocLocal(SymbolId sym, TypeId type) -> LocalAllocation;
  auto AllocTemp(TypeId type) -> mir::PlaceId;

  // Throws InternalError if symbol not found (compiler bug, not user error).
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;

  // Resolve a function symbol to its pre-allocated mir::FunctionId.
  // Throws InternalError if symbol not found (HIR guarantees all functions
  // are pre-allocated, so missing = compiler bug).
  [[nodiscard]] auto ResolveCallee(SymbolId sym) const -> mir::FunctionId;

  [[nodiscard]] auto GetBitType() const -> TypeId {
    return builtin_types.bit_type;
  }

  [[nodiscard]] auto GetOffsetType() const -> TypeId {
    return builtin_types.offset_type;
  }

  [[nodiscard]] auto GetStringType() const -> TypeId {
    return builtin_types.string_type;
  }
};

}  // namespace lyra::lowering::hir_to_mir
