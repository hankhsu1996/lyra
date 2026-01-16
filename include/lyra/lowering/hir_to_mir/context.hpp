#pragma once

#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

struct SymbolIdHash {
  auto operator()(SymbolId id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

using PlaceMap = std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>;

// Context for lowering within a process or function activation.
// module_places is module-scoped (read-only), local_places is process-scoped.
struct Context {
  mir::Arena* mir_arena;

  const hir::Arena* hir_arena;
  const TypeArena* type_arena;
  const ConstantArena* constant_arena;
  const SymbolTable* symbol_table;

  const PlaceMap* module_places;
  PlaceMap local_places;

  int next_local_id = 0;
  int next_temp_id = 0;

  auto AllocLocal(SymbolId sym, TypeId type) -> mir::PlaceId;
  auto AllocTemp(TypeId type) -> mir::PlaceId;
  auto LookupPlace(SymbolId sym) const -> mir::PlaceId;
};

}  // namespace lyra::lowering::hir_to_mir
