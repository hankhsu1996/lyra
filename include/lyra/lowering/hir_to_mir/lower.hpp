#pragma once

#include <memory>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

struct LoweringInput {
  const hir::Design* design = nullptr;
  const hir::Arena* hir_arena = nullptr;
  TypeArena* type_arena = nullptr;
  const ConstantArena* constant_arena = nullptr;
  const SymbolTable* symbol_table = nullptr;
  BuiltinTypes builtin_types;
};

struct LoweringResult {
  mir::Design design;
  std::unique_ptr<mir::Arena> mir_arena;
};

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult;

}  // namespace lyra::lowering::hir_to_mir
