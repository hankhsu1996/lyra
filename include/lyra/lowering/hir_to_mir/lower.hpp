#pragma once

#include <memory>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

struct LoweringInput {
  const hir::Design* design = nullptr;
  const hir::Arena* hir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  const ConstantArena* constant_arena = nullptr;
  const SymbolTable* symbol_table = nullptr;
  TypeId bit_type;     // 1-bit 2-state for bool results
  TypeId offset_type;  // 32-bit 2-state unsigned for offset arithmetic
  TypeId string_type;  // string type for warning messages
};

struct LoweringResult {
  mir::Design design;
  std::unique_ptr<mir::Arena> mir_arena;
};

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult;

}  // namespace lyra::lowering::hir_to_mir
