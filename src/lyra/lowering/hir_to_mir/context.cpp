#include "lyra/lowering/hir_to_mir/context.hpp"

#include <cassert>
#include <format>
#include <stdexcept>
#include <utility>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto InternBuiltinTypes(TypeArena& arena) -> BuiltinTypes {
  return BuiltinTypes{
      .bit_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = false}),
      .offset_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = false, .is_four_state = false}),
      .string_type = arena.Intern(TypeKind::kString, std::monostate{}),
  };
}

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

auto Context::ResolveCallee(SymbolId sym) const -> mir::FunctionId {
  if (symbol_to_mir_function == nullptr) {
    throw common::InternalError(
        "ResolveCallee", "function map not set in context");
  }
  auto it = symbol_to_mir_function->find(sym);
  if (it != symbol_to_mir_function->end()) {
    return it->second;
  }
  const Symbol& s = (*symbol_table)[sym];
  throw std::runtime_error(
      std::format("unresolved function '{}' in MIR lowering", s.name));
}

}  // namespace lyra::lowering::hir_to_mir
