#include "lyra/lowering/hir_to_mir/lower.hpp"

#include "lyra/lowering/hir_to_mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult {
  auto mir_arena = std::make_unique<mir::Arena>();

  mir::Design design = LowerDesign(*input.design, input, *mir_arena);

  return LoweringResult{
      .design = std::move(design),
      .mir_arena = std::move(mir_arena),
  };
}

}  // namespace lyra::lowering::hir_to_mir
