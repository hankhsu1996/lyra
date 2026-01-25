#include "lyra/lowering/hir_to_mir/lower.hpp"

#include <variant>

#include "lyra/common/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/verify.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Verify all MIR produced by lowering. Called once at the end of the pipeline.
// Failures throw InternalError (compiler bug - we emitted invalid MIR).
void VerifyLoweredMir(
    const mir::Design& design, const mir::Arena& arena,
    const TypeArena& type_arena) {
  for (const auto& element : design.elements) {
    std::visit(
        Overloaded{
            [&](const mir::Module& mod) {
              for (mir::FunctionId fid : mod.functions) {
                mir::VerifyFunction(arena[fid], arena, type_arena);
              }
              for (mir::ProcessId pid : mod.processes) {
                mir::VerifyProcess(arena[pid], arena, type_arena);
              }
            },
            [&](const mir::Package& pkg) {
              for (mir::FunctionId fid : pkg.functions) {
                mir::VerifyFunction(arena[fid], arena, type_arena);
              }
            },
        },
        element);
  }
}

}  // namespace

auto LowerHirToMir(const LoweringInput& input) -> LoweringResult {
  auto mir_arena = std::make_unique<mir::Arena>();
  OriginMap origin_map;

  LoweringInput full_input = input;
  full_input.builtin_types = InternBuiltinTypes(*input.type_arena);

  mir::Design design =
      LowerDesign(*input.design, full_input, *mir_arena, &origin_map);

  // Single verification gate at the end of lowering
  VerifyLoweredMir(design, *mir_arena, *input.type_arena);

  return LoweringResult{
      .design = std::move(design),
      .mir_arena = std::move(mir_arena),
      .origin_map = std::move(origin_map),
  };
}

}  // namespace lyra::lowering::hir_to_mir
