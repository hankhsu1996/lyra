#include "lyra/lowering/mir_to_llvm/instruction.hpp"

#include "lyra/common/overloaded.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerDisplayEffect(Context& context, const mir::DisplayEffect& display) {
  auto& builder = context.GetBuilder();

  // Build the format string from the display effect arguments
  std::string format_str;

  for (const auto& operand : display.args) {
    // Check if it's a string constant
    if (operand.kind == mir::Operand::Kind::kConst) {
      const auto& constant = std::get<Constant>(operand.payload);
      if (std::holds_alternative<StringConstant>(constant.value)) {
        format_str += std::get<StringConstant>(constant.value).value;
      } else {
        format_str += "%d";  // Placeholder for non-string
      }
    } else {
      format_str += "%d";  // Placeholder for non-constant
    }
  }

  // Add newline if needed
  if (display.append_newline) {
    format_str += "\n";
  }

  // Create global string constant
  auto* format_const = builder.CreateGlobalStringPtr(format_str);

  // Call printf
  builder.CreateCall(context.GetPrintfFunction(), {format_const});
}

void LowerEffectOp(Context& context, const mir::EffectOp& effect_op) {
  std::visit(
      Overloaded{
          [&context](const mir::DisplayEffect& display) {
            LowerDisplayEffect(context, display);
          },
          [](const mir::SeverityEffect& /*severity*/) {
            // TODO: Handle severity effects
          },
      },
      effect_op);
}

}  // namespace

void LowerInstruction(Context& context, const mir::Instruction& instruction) {
  std::visit(
      Overloaded{
          [](const mir::Assign& /*assign*/) {
            // TODO: Handle assignments
          },
          [](const mir::Compute& /*compute*/) {
            // TODO: Handle computations
          },
          [](const mir::GuardedAssign& /*guarded*/) {
            // TODO: Handle guarded assignments
          },
          [&context](const mir::Effect& effect) {
            LowerEffectOp(context, effect.op);
          },
      },
      instruction);
}

}  // namespace lyra::lowering::mir_to_llvm
