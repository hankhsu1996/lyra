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
  const hir::Design& design;
  const hir::Arena& hir_arena;
  const TypeArena& type_arena;
  const ConstantArena& constant_arena;
  const SymbolTable& symbol_table;
  TypeId bit_type;     // 1-bit 2-state for bool results
  TypeId offset_type;  // 32-bit 2-state unsigned for offset arithmetic
};

struct LoweringResult {
  mir::Design design;
  std::unique_ptr<mir::Arena> mir_arena;
};

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult;

}  // namespace lyra::lowering::hir_to_mir
