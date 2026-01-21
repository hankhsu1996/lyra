#include "lyra/lowering/mir_to_llvm/instruction.hpp"

#include "lyra/common/overloaded.hpp"
#include "lyra/lowering/mir_to_llvm/instruction_display.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerEffectOp(Context& context, const mir::EffectOp& effect_op) {
  std::visit(
      Overloaded{
          [&context](const mir::DisplayEffect& display) {
            LowerDisplayEffect(context, display);
          },
          [](const mir::SeverityEffect& /*severity*/) {
            // TODO(hankhsu): Handle severity effects
          },
      },
      effect_op);
}

}  // namespace

void LowerInstruction(Context& context, const mir::Instruction& instruction) {
  std::visit(
      Overloaded{
          [](const mir::Assign& /*assign*/) {
            // TODO(hankhsu): Handle assignments
          },
          [](const mir::Compute& /*compute*/) {
            // TODO(hankhsu): Handle computations
          },
          [](const mir::GuardedAssign& /*guarded*/) {
            // TODO(hankhsu): Handle guarded assignments
          },
          [&context](const mir::Effect& effect) {
            LowerEffectOp(context, effect.op);
          },
      },
      instruction);
}

}  // namespace lyra::lowering::mir_to_llvm
