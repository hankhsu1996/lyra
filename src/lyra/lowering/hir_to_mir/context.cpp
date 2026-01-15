#include "lyra/lowering/hir_to_mir/context.hpp"

#include <string>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto Context::AllocLocal(SymbolId sym, TypeId type) -> mir::PlaceId {
  mir::Place place{
      .root =
          mir::PlaceRoot{
              .kind = mir::PlaceRoot::Kind::kLocal,
              .id = next_local_id++,
              .type = type,
          },
      .projections = {},
  };
  mir::PlaceId place_id = mir_arena.AddPlace(std::move(place));
  symbol_places[sym] = place_id;
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
  return mir_arena.AddPlace(std::move(place));
}

auto Context::LookupPlace(SymbolId sym) const -> mir::PlaceId {
  auto it = symbol_places.find(sym);
  if (it == symbol_places.end()) {
    std::string msg = "symbol ";
    msg += std::to_string(sym.value);
    msg += " not found in place mapping";
    throw common::InternalError("HIR to MIR lowering", msg);
  }
  return it->second;
}

}  // namespace lyra::lowering::hir_to_mir
