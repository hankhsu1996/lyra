#include "lyra/lowering/hir_to_mir/context.hpp"

#include <cassert>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto Context::AllocLocal(SymbolId sym, TypeId type) -> mir::PlaceId {
  assert(
      (module_places == nullptr || !module_places->contains(sym)) &&
      "AllocLocal called for symbol that already has a module place");

  mir::Place place{
      .root =
          mir::PlaceRoot{
              .kind = mir::PlaceRoot::Kind::kLocal,
              .id = next_local_id++,
              .type = type,
          },
      .projections = {},
  };
  mir::PlaceId place_id = mir_arena->AddPlace(std::move(place));
  local_places[sym] = place_id;
  return place_id;
}

auto Context::AllocTemp(TypeId type) -> mir::PlaceId {
  mir::Place place{
      .root =
          mir::PlaceRoot{
              .kind = mir::PlaceRoot::Kind::kTemp,
              .id = next_temp_id++,
              .type = type,
          },
      .projections = {},
  };
  return mir_arena->AddPlace(std::move(place));
}

auto Context::LookupPlace(SymbolId sym) const -> mir::PlaceId {
  if (module_places != nullptr) {
    auto it = module_places->find(sym);
    if (it != module_places->end()) {
      return it->second;
    }
  }

  auto it = local_places.find(sym);
  if (it != local_places.end()) {
    return it->second;
  }

  throw common::InternalError(
      "HIR to MIR lowering",
      std::format("symbol {} not found in place mapping", sym.value));
}

}  // namespace lyra::lowering::hir_to_mir
