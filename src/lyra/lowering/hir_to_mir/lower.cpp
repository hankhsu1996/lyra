#include "lyra/lowering/hir_to_mir/lower.hpp"

#include "lyra/lowering/hir_to_mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult {
  auto mir_arena = std::make_unique<mir::Arena>();
  OriginMap origin_map;

  LoweringInput full_input = input;
  full_input.builtin_types = InternBuiltinTypes(*input.type_arena);

  mir::Design design =
      LowerDesign(*input.design, full_input, *mir_arena, &origin_map);

  return LoweringResult{
      .design = std::move(design),
      .mir_arena = std::move(mir_arena),
      .origin_map = std::move(origin_map),
  };
}

}  // namespace lyra::lowering::hir_to_mir
