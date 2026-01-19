#pragma once

#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

struct SymbolIdHash {
  auto operator()(SymbolId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

using PlaceMap = std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>;

// Map SymbolId → mir::FunctionId (pre-allocated for recursion support)
using SymbolToMirFunctionMap =
    std::unordered_map<SymbolId, mir::FunctionId, SymbolIdHash>;

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

  TypeId bit_type;

  // Function-specific: map symbols to MIR function IDs (for call lowering)
  const SymbolToMirFunctionMap* symbol_to_mir_function = nullptr;

  // Function-specific: local 0 for non-void return value
  mir::PlaceId return_place = mir::kInvalidPlaceId;

  auto AllocLocal(SymbolId sym, TypeId type) -> mir::PlaceId;
  auto AllocTemp(TypeId type) -> mir::PlaceId;

  // Throws InternalError if symbol not found (compiler bug, not user error).
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;

  // Look up mir::FunctionId for a function symbol. Returns invalid if not
  // found.
  [[nodiscard]] auto LookupFunction(SymbolId sym) const -> mir::FunctionId;

  [[nodiscard]] auto GetBitType() const -> TypeId {
    return bit_type;
  }
};

}  // namespace lyra::lowering::hir_to_mir
