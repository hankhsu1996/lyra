#pragma once

#include <span>
#include <string_view>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/xir/compilation_unit.hpp"

namespace lyra::hir {
struct ModuleBody;
class Arena;
}  // namespace lyra::hir

namespace lyra {
class TypeArena;
class SymbolTable;
class ConstantArena;
}  // namespace lyra

namespace lyra::lowering::hir_to_xir {

// Input adapter for the first-slice HIR-to-XIR lowering.
//
// All pointer fields are mandatory (must not be null). LowerToXir validates
// at entry and fails immediately on null.
//
// constant_arena is carried for downstream projection (step 4) but is
// intentionally unused during XIR construction itself.
struct CompilationUnitInput {
  const hir::ModuleBody* body = nullptr;
  const hir::Arena* hir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  const SymbolTable* symbol_table = nullptr;
  // Carried for projection (step 4). Unused during XIR construction.
  const ConstantArena* constant_arena = nullptr;
  std::string_view compilation_unit_name;
  std::span<const SymbolId> variable_symbols;
};

auto LowerToXir(const CompilationUnitInput& input)
    -> lyra::Result<xir::CompilationUnit>;

}  // namespace lyra::lowering::hir_to_xir
