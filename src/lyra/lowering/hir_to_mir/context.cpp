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
#include "lyra/mir/dpi_verify.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
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
      .i8_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 8, .is_signed = false, .is_four_state = false}),
      .i16_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 16, .is_signed = false, .is_four_state = false}),
  };
}

auto Context::AllocLocal(SymbolId sym, TypeId type) -> LocalAllocation {
  if (body_places != nullptr && body_places->contains(sym)) {
    throw common::InternalError(
        "AllocLocal",
        std::format("symbol {} already has a body place", sym.value));
  }
  if (design_places != nullptr && design_places->contains(sym)) {
    throw common::InternalError(
        "AllocLocal",
        std::format("symbol {} already has a design place", sym.value));
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
  int temp_id = next_temp_id++;
  mir::Place place{
      .root =
          mir::PlaceRoot{
              .kind = mir::PlaceRoot::Kind::kTemp,
              .id = temp_id,
              .type = type,
          },
      .projections = {},
  };
  // Populate type table during allocation, not post-collection
  temp_types.push_back(type);
  // Record in temp_metadata as PlaceTemp
  temp_metadata.push_back({.kind = mir::TempKind::kPlace, .type = type});
  return mir_arena->AddPlace(std::move(place));
}

auto Context::AllocValueTemp(TypeId type) -> int {
  int temp_id = next_temp_id++;
  // Populate deprecated type table for compatibility
  temp_types.push_back(type);
  // Record in temp_metadata as ValueTemp
  temp_metadata.push_back({.kind = mir::TempKind::kValue, .type = type});
  return temp_id;
}

auto Context::LookupPlace(SymbolId sym) const -> mir::PlaceId {
  // Lookup order: local -> body -> design-global
  auto it = local_places.find(sym);
  if (it != local_places.end()) {
    return it->second;
  }

  if (body_places != nullptr) {
    auto body_it = body_places->find(sym);
    if (body_it != body_places->end()) {
      return body_it->second;
    }
  }

  if (design_places != nullptr) {
    auto design_it = design_places->find(sym);
    if (design_it != design_places->end()) {
      // During body lowering (design_arena set): create a body-local Place
      // with the design-global root. This is not import -- it is normal
      // body-local MIR that references design-global storage through an
      // explicit kDesignGlobal root.
      //
      // Invariant: design-global places for top-level declarations have
      // only a root (kDesignGlobal, slot_id, type) and empty projections.
      // The copy reads canonical root data, not deep MIR structure.
      if (design_arena != nullptr) {
        auto cache_it = design_place_cache.find(sym);
        if (cache_it != design_place_cache.end()) {
          return cache_it->second;
        }
        const mir::Place& design_place = (*design_arena)[design_it->second];
        mir::PlaceId body_place_id =
            mir_arena->AddPlace(mir::Place(design_place));
        design_place_cache[sym] = body_place_id;
        return body_place_id;
      }
      // Design-level lowering: return design-arena PlaceId directly.
      return design_it->second;
    }
  }

  throw common::InternalError(
      "HIR to MIR lowering",
      std::format("symbol {} not found in place mapping", sym.value));
}

namespace {

auto BuildDpiSignature(const DpiImportInfo& dpi) -> mir::DpiSignature {
  mir::DpiSignature sig;
  sig.result = {
      .sv_type = dpi.return_type_id,
      .abi_type = dpi.return_abi_type,
      .kind = dpi.return_abi_type == DpiAbiTypeClass::kVoid
                  ? mir::DpiReturnKind::kVoid
                  : mir::DpiReturnKind::kDirectValue,
  };
  sig.params.reserve(dpi.params.size());
  for (const auto& p : dpi.params) {
    sig.params.push_back({
        .sv_type = p.type_id,
        .abi_type = p.abi_type,
        .direction = p.direction,
        .passing = mir::GetDpiPassingMode(p.direction, p.abi_type),
    });
  }
  mir::ValidateDpiSignatureContract(sig, "BuildDpiSignature");
  return sig;
}

}  // namespace

auto Context::ResolveDpiImport(SymbolId sym) const
    -> std::optional<mir::DpiImportRef> {
  if (dpi_imports == nullptr) {
    return std::nullopt;
  }
  const auto* dpi = dpi_imports->Find(sym);
  if (dpi == nullptr) {
    return std::nullopt;
  }
  // Invariant: DPI import symbols must not collide with function maps.
  if (design_functions != nullptr && design_functions->contains(sym)) {
    throw common::InternalError(
        "ResolveDpiImport",
        std::format(
            "symbol {} found in both DPI imports and design functions",
            sym.value));
  }
  if (symbol_to_mir_function != nullptr &&
      symbol_to_mir_function->contains(sym)) {
    throw common::InternalError(
        "ResolveDpiImport",
        std::format(
            "symbol {} found in both DPI imports and body-local functions",
            sym.value));
  }
  return mir::DpiImportRef{
      .symbol = sym,
      .c_name = dpi->c_name,
      .signature = BuildDpiSignature(*dpi),
      .is_context = dpi->is_context,
  };
}

auto Context::ResolveCallTarget(SymbolId sym) const -> mir::Callee {
  // DPI imports are resolved via ResolveDpiImport, not through Callee.
  // Check for collision anyway.
  if (dpi_imports != nullptr && dpi_imports->Find(sym) != nullptr) {
    throw common::InternalError(
        "ResolveCallTarget",
        std::format(
            "DPI import symbol {} should not reach generic call resolution",
            sym.value));
  }

  // During body lowering: check design-global functions.
  if (design_functions != nullptr) {
    auto design_it = design_functions->find(sym);
    if (design_it != design_functions->end()) {
      return mir::DesignFunctionRef{.symbol = sym};
    }
  }
  // Body-local or design-level: resolve as arena-local FunctionId
  return ResolveCallee(sym);
}

auto Context::ResolveCallSignature(const mir::Callee& callee) const
    -> const mir::FunctionSignature& {
  return std::visit(
      common::Overloaded{
          [this](mir::FunctionId func_id) -> const mir::FunctionSignature& {
            return (*mir_arena)[func_id].signature;
          },
          [this](const mir::DesignFunctionRef& ref)
              -> const mir::FunctionSignature& {
            auto it = design_functions->find(ref.symbol);
            return (*design_arena)[it->second].signature;
          },
          [](SystemTfOpcode) -> const mir::FunctionSignature& {
            throw common::InternalError(
                "ResolveCallSignature",
                "cannot resolve signature for SystemTfOpcode");
          },
      },
      callee);
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
