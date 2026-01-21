#include "lyra/lowering/hir_to_mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerPackage(
    const hir::Package& package, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> mir::Package {
  (void)package;
  (void)input;
  (void)mir_arena;
  (void)origin_map;

  // Package lowering is a placeholder for now
  return mir::Package{};
}

}  // namespace lyra::lowering::hir_to_mir
