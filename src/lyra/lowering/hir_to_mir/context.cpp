#include "lyra/lowering/hir_to_mir/context.hpp"

#include <cstdint>
#include <format>
#include <utility>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto InternBuiltinTypes(TypeArena& arena) -> BuiltinTypes {
  return BuiltinTypes{
      .bit_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = false}),
      .logic_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = true}),
      .offset_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = false, .is_four_state = false}),
      .string_type = arena.Intern(TypeKind::kString, std::monostate{}),
      .void_type = arena.Intern(TypeKind::kVoid, std::monostate{}),
  };
}

auto Context::AllocLocal(SymbolId sym, TypeId type) -> LocalAllocation {
  if (module_places != nullptr && module_places->contains(sym)) {
    throw common::InternalError(
        "AllocLocal",
        std::format("symbol {} already has a module place", sym.value));
  }

  auto local_slot = static_cast<uint32_t>(next_local_id++);

  mir::Place place{
      .root =
          mir::PlaceRoot{
              .kind = mir::PlaceRoot::Kind::kLocal,
              .id = static_cast<int>(local_slot),
              .type = type,
          },
      .projections = {},
  };
  mir::PlaceId place_id = mir_arena->AddPlace(std::move(place));
  local_places[sym] = place_id;

  // Populate type table during allocation, not post-collection
  local_types.push_back(type);

  return LocalAllocation{.place = place_id, .local_slot = local_slot};
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
  // Populate type table during allocation, not post-collection
  temp_types.push_back(type);
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
  throw common::InternalError(
      "hir_to_mir",
      std::format("unresolved function '{}' in MIR lowering", s.name));
}

}  // namespace lyra::lowering::hir_to_mir
