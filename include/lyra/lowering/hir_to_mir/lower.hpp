#pragma once

#include <memory>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
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
  const ast_to_hir::DesignBindingPlan* binding_plan = nullptr;
};

struct LoweringResult {
  mir::Design design;
  std::unique_ptr<mir::Arena> mir_arena;
  OriginMap origin_map;
};

auto LowerHirToMir(const LoweringInput& input) -> Result<LoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
