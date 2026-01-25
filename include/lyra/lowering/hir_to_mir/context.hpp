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
struct DesignDeclarations {
  PlaceMap design_places;
  SymbolToMirFunctionMap functions;
  size_t num_design_slots = 0;
  // Slot table: indexed by design slot ID, contains TypeId for each slot.
  // Ordering is ABI: packages first (in element order), then all module
  // instances (in BFS elaboration order).
  std::vector<TypeId> slot_table;
};

// Read-only view into declaration artifacts for lower-level helpers
// (LowerProcess, LowerFunctionBody). Ensures the frozen boundary is
// consistent throughout the lowering layer.
struct DeclView {
  const PlaceMap* places;
  const SymbolToMirFunctionMap* functions;
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

  BuiltinTypes builtin_types;

  // Function-specific: map symbols to MIR function IDs (for call lowering)
  const SymbolToMirFunctionMap* symbol_to_mir_function = nullptr;

  // Function-specific: local 0 for non-void return value
  mir::PlaceId return_place = mir::kInvalidPlaceId;

  auto AllocLocal(SymbolId sym, TypeId type) -> mir::PlaceId;
  auto AllocTemp(TypeId type) -> mir::PlaceId;

  // Throws InternalError if symbol not found (compiler bug, not user error).
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;

  // Resolve a function symbol to its pre-allocated mir::FunctionId.
  // Throws std::runtime_error with function name if symbol not found.
  // Throws InternalError if function map pointer is null (programmer bug).
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
